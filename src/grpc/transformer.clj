(ns grpc.transformer
  (:require [clojure.walk])
  (:import [io.grpc Server ServerBuilder]))


(defn find-method [clz mname & params]
  (let [ms (filter #(and (= mname (.getName %))
                         (= (count params) (count (.getParameterTypes %))))
                   (.getDeclaredMethods clz))]
    (first ms)))

(defn find-methods [clz mname ]
  (filter #(= mname (.getName %)) (.getDeclaredMethods clz)))


(defn- display-method [clz]
  (map #(.getName %) (.getDeclaredMethods clz)))



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

(def TYPE-MAP {:STRING :BASE
               :ENUM :ENUM
               :INT :BASE
               :DOUBLE :BASE
               :BOOLEAN :BASE
               :LONG :BASE
               :MAP :BASE
               :MESSAGE :MESSAGE})


(defmulti build (fn [type builder method name val] (TYPE-MAP type)))


(defn message-field-descriptor->clz [builder field-descriptor]
  (let [pre  (if (.isRepeated field-descriptor) "add" "get")
        builder-name (str (field-builder-method-name pre field-descriptor) "Builder")
        method (find-method (class builder) builder-name)
        new-builder-name (some-> (.invoke method builder nil)
                                 class
                                 .getName)]
    (if (clojure.string/ends-with? new-builder-name "$Builder")
      (Class/forName (.substring new-builder-name 0 (- (count new-builder-name) 8))))))

(defmethod build :REPEAT [type builder method field-descriptor vals]
  (doseq [v vals]
    (.invoke method builder (object-array [v]))))


(defmethod build :BASE [type builder method field-descriptor vals]
  (when vals
    (.clearField builder field-descriptor)
    (.invoke method builder (object-array [vals]))))


#_(defmethod build :ENUM [type builder method field-descriptor vals]
    (when vals
      (prn "1111" vals)
      (prn "222" (name vals))
      (prn "3333" method)
      (.clearField builder field-descriptor)
      (.invoke method builder (object-array [(name vals)]))))


(defmethod build :ENUM [type builder method field-descriptor vals]
  (when vals
    (let [enum-field-descripter (.getEnumType field-descriptor)]
      (if (.isRepeated field-descriptor)
        (doseq [v (map #(.findValueByName enum-field-descripter %) (map name vals))]
          ;;(.setRepeatedField builder field-descriptor 0 v)
          (.addRepeatedField builder field-descriptor v)
          )
        (.setField builder field-descriptor (.findValueByName enum-field-descripter (name vals)))))))


#_(defmethod build :INT [type builder method field-descriptor vals]
    (when vals
      (if (.isRepeated field-descriptor)
        (doseq [v vals]
          (.addRepeatedField builder field-descriptor (.intValue v)))
        (.setField builder field-descriptor (.intValue vals)))))

#_(defmethod build :DOUBLE [type builder method field-descriptor vals]
    (when vals
      (if (.isRepeated field-descriptor)
        (doseq [v vals]
          (.addRepeatedField builder field-descriptor (.doubleValue v)))
        (.setField builder field-descriptor (.doubleValue vals)))))

#_(defmethod build :BOOLEAN [type builder method field-descriptor vals]
    (when vals
      (if (.isRepeated field-descriptor)
        (doseq [v vals]
          (.addRepeatedField builder field-descriptor v))
        (.setField builder field-descriptor vals))))

#_(defmethod build :LONG [type builder method field-descriptor vals]
    (when vals
      (if (.isRepeated field-descriptor)
        (doseq [v vals]
          (.addRepeatedField builder field-descriptor v))
        (.setField builder field-descriptor vals))))

(declare ->message)

(defmethod build :MESSAGE [type builder method field-descriptor vals]
  (when vals
    (if (.isRepeated field-descriptor)
      (let [clz (message-field-descriptor->clz builder field-descriptor)]
        (.clearField builder field-descriptor)
        (doseq [v vals]
          (let [inner-message (->message v clz)]
            (.addRepeatedField builder field-descriptor inner-message))))
      (let [clz (message-field-descriptor->clz builder field-descriptor)
            inner-message (->message vals clz)]
        (.setField builder field-descriptor inner-message)))))


;;


#_(defmethod build :MAP [type builder method field-descriptor m]
    (when m
      (.clearField builder field-descriptor)
      (.invoke method builder (object-array [m]))))


(defn parse-field-descriptor [builder field-descriptor]
  (let [type (if (.isMapField field-descriptor)
               :MAP
               (keyword (str (.getJavaType field-descriptor))))
        builder-name (.getName (class builder))
        build-method-str (cond (and (.isRepeated field-descriptor) (= :MAP type))
                               (field-builder-method-name "putAll" field-descriptor)
                               (.isRepeated field-descriptor)
                               (field-builder-method-name "add" field-descriptor)
                               :else
                               (field-builder-method-name "set" field-descriptor))]

    {:type type
     :name (keyword (.getName field-descriptor))
     :build-method-str build-method-str
     :build-method (find-method (class builder) build-method-str 1)
     :repeated? (.isRepeated field-descriptor)
     :get-method (let [gstr (str (field-builder-method-name "get" field-descriptor) (when (.isRepeated field-descriptor) "List"))]
                   (find-method (class builder) gstr))
     :java-type (.getJavaType field-descriptor)}))

(defn ->message [o clz]
  (let [builder (find-builder clz)
        descriptor (find-builder-descriptor clz)]
    (doseq [field (.getFields descriptor)]
      (let [{:keys [type build-method name repeated?]} (parse-field-descriptor builder field)]
        (build type builder build-method field (get o name))))
    (.build builder)))



(declare <-message)


(defn parse-message-value [v]
  (cond (instance? com.google.protobuf.Descriptors$EnumValueDescriptor v)
        (keyword (.getName v))
        (instance? com.google.protobuf.Message v)
        (<-message v)
        (instance? java.util.List v)
        (let [vv (java.util.ArrayList.)
              _ (.addAll vv v)]
          (into [] (map parse-message-value vv)))
        :default
        v))


(defn- change-map-field [v]
  (let [vv (java.util.ArrayList.)
        _ (.addAll vv v)]
    (into {} (map (fn [i] {(:key i) (:value i)}) (map parse-message-value vv)))))

(defn <-message [m]
  (let [mm (java.util.HashMap.)]
    (.putAll mm (.getAllFields m))
    (into {} (for [[k v] mm]
               (let [new-key (keyword (.getName k))
                     new-val (if (.isMapField k)
                               (change-map-field v)
                               (parse-message-value v))]
                 [new-key new-val])))))
