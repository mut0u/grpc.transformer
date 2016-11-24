(ns grpc.transformer
  (:import [io.grpc Server ServerBuilder]))



(defn find-method [clz mname & params]
  (let [ms (filter #(and (= mname (.getName %))
                         (= (count params) (count (.getParameterTypes %))))
                   (.getDeclaredMethods clz))]
    (first ms)))


(defn- call-static-class-method [clz mname]
  (let [method (find-method clz mname)]
    (.invoke method nil nil)))

(defn find-builder [clz]
  (.invoke (find-method  clz "newBuilder") nil nil))


(defn find-builder-descriptor [clz]
  (let [method (find-method clz "getDescriptor")]
    (.invoke method nil nil)))


(defn field-builder-method-name [get-or-set-pre field-descriptor]
  (let [json-name (.getJsonName field-descriptor)]
    (str get-or-set-pre
         (.toUpperCase (.substring json-name 0 1))
         (.substring json-name 1))))

(defmulti build (fn [type builder name val] type))

(def type-map {:BOOLEAN java.lang.Boolean
               :DOUBLE java.lang.Double
               :ENUM java.lang.Enum
               :FLOAT java.lang.Float
               :INT java.lang.Integer
               :LONG java.lang.Long
               :STRING java.lang.String
               ;;;BYTE_STRING
               })




(defn message-field-descriptor->clz [builder field-descriptor]
  (let [pre  (if (.isRepeated field-descriptor) "add" "get")
        builder-name (str (field-builder-method-name pre field-descriptor) "Builder")
        method (find-method (class builder) builder-name)
        new-builder-name (some-> (.invoke method builder nil)
                                 class
                                 .getName)]
    (if (clojure.string/ends-with? new-builder-name "$Builder")
      (Class/forName (.substring new-builder-name 0 (- (count new-builder-name) 8))))))




(defmethod build :STRING [type builder field-descriptor vals]
  (when vals
    (if (.isRepeated field-descriptor)
      (doseq [v vals]
        (.addRepeatedField builder field-descriptor v))
      (.setField builder field-descriptor vals))))

(defmethod build :ENUM [type builder field-descriptor vals]
  (when vals
    (let [enum-field-descripter (.getEnumType field-descriptor)]
      (if (.isRepeated field-descriptor)
        (doseq [v (map #(.findValueByName enum-field-descripter %) vals)]
          (.addRepeatedField builder field-descriptor v))
        (.setField builder field-descriptor (.findValueByName enum-field-descripter vals))))))

(defmethod build :INT [type builder field-descriptor vals]
  (when vals
    (.setField builder field-descriptor vals)))

(defmethod build :LONG [type builder field-descriptor vals]
  (when vals
    (.setField builder field-descriptor vals)))

(declare ->message)

(defmethod build :MESSAGE [type builder field-descriptor vals]
  (when vals
    (if (.isRepeated field-descriptor)
      (let [clz (message-field-descriptor->clz builder field-descriptor)]
        (doseq [v vals]
          (let [inner-message (->message v clz)]
            (.addRepeatedField builder field-descriptor inner-message))))
      (let [clz (message-field-descriptor->clz builder field-descriptor)
            inner-message (->message vals clz)]
        (.setField builder field-descriptor inner-message)))))


(defn parse-field-descriptor [builder field-descriptor]
  (let [build-method-str (if (.isRepeated field-descriptor)
                           (field-builder-method-name "add" field-descriptor)
                           (field-builder-method-name "set" field-descriptor))]
    {:type (keyword (str (.getJavaType field-descriptor)))
     :name (keyword (.getName field-descriptor))
     :build-method-str build-method-str
     :build-method (find-method (class builder) build-method-str 1)
     :repeated? (.isRepeated field-descriptor)
     :get-method (str (field-builder-method-name "get" field-descriptor) (when (.isRepeated field-descriptor) "List"))
     :java-type (.getJavaType field-descriptor)
     :type1 (.getType field-descriptor)}))

(defn ->message [o clz]
  (let [builder (find-builder clz)
        descriptor (find-builder-descriptor clz)]
    (doseq [field (.getFields descriptor)]
      (let [{:keys [type build-method name repeated?]} (parse-field-descriptor builder field)]
        (build type builder field (get o name))))
    (.build builder)))
