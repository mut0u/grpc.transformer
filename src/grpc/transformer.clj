(ns grpc.transformer
  (:require [clojure.walk])
  (:import [io.grpc Server ServerBuilder]))


(defn find-method [clz mname & params]
  (some-> (filter #(and (= mname (.getName %))
                        (= (count params) (count (.getParameterTypes %))))
                  (.getDeclaredMethods clz))
          first))

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
               :INT :BASE
               :DOUBLE :BASE
               :BOOLEAN :BASE
               :LONG :BASE
               :MAP :BASE
               :ENUM :ENUM
               :MESSAGE :MESSAGE
               :REPEAT  :REPEAT})

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

(defmethod build :ENUM [type builder method field-descriptor vals]
  (when vals
    (let [enum-field-descripter (.getEnumType field-descriptor)]
      (if (.isRepeated field-descriptor)
        (doseq [v (map #(.findValueByName enum-field-descripter %) (map name vals))]
          (.addRepeatedField builder field-descriptor v))
        (.setField builder field-descriptor (.findValueByName enum-field-descripter (name vals)))))))

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

(defn parse-field-descriptor [builder field-descriptor]
  (let [type (cond (.isMapField field-descriptor) :MAP
                   (and (.isRepeated field-descriptor)
                        (= :BASE (TYPE-MAP (keyword (str (.getJavaType field-descriptor)))))) :REPEAT
                   :else (keyword (str (.getJavaType field-descriptor))))
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
        :else
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
