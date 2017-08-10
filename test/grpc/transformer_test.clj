(ns grpc.transformer-test
  (:require [clojure.test :refer :all]
            [grpc.transformer :refer :all])
  (:import [michael.test Demo$StringMessage Demo$EnumMessage Demo$RepeatedEnumMessage Demo$IntMessage Demo$RepeatedStringMessage Demo$DemoMessage Demo$Message1 Demo$NestedMessage Demo$MapMessage]))


(defn- display-method [clz]
  (map #(.getName %) (.getDeclaredMethods clz)))

(defn find-methods [clz mname ]
  (filter #(= mname (.getName %)) (.getDeclaredMethods clz)))

(defn- call-static-class-method [clz mname]
  (let [method (find-method clz mname)]
    (.invoke method nil nil)))


#_(deftest test-repeated-string-message
    (testing "test clojure message transfer to protobuf message "
      (let [clz Demo$RepeatedStringMessage
            message-value ["hello" "world"]
            message (->message {:slist message-value} clz)]
        (is (= michael.test.Demo$RepeatedStringMessage (type message)))
        (is (= message-value (.getSlist message))))))



(deftest test-base
  (testing "test base message transformer"
    (is (let [o {:s "aa"} clz Demo$StringMessage] (= o (<-message (->message o clz)))))
    (is (let [o {:s ""} clz Demo$StringMessage] (= {} (<-message (->message o clz)))))
    (is (let [o {:s1 "a"} clz Demo$StringMessage] (= {} (<-message (->message o clz)))))
    (is (let [clz Demo$EnumMessage o {:bar :BAR_B}] (= o (<-message (->message o clz)))))
    (is (let [clz Demo$IntMessage o {:i 66}] (= o (<-message (->message o clz)))))
    (is (let [clz Demo$Message1 o {:str "hello world" :i 76}] (= o (<-message (->message o clz)))))
    (is (let [clz Demo$DemoMessage o {:m2 {:s "hello world0" :i 1}}] (= o (<-message (->message o clz)))))
    (is (let [clz Demo$NestedMessage o {:n {:n2 {:n1 {:str "BBB"} :i {:s "aaaa"}}}}] (= o (<-message (->message o clz)))))
    (is (let [clz Demo$MapMessage o {:m {"a" "aaa" "b" "bb" "c" "c" }}] (= o (<-message (->message o clz)))))
    ))


(deftest test-repeated
  (testing "test the repeated message transformer "
    (is (let [o {:slist ["a" "d" "e"]} clz Demo$RepeatedStringMessage] (= o (<-message (->message o clz)))))
    (is (let [clz Demo$RepeatedEnumMessage o {:bar [:BAR_B :BAR_C :BAR_A :BAR_B]}] (->message o clz)))
    (is (let [clz Demo$DemoMessage o {:m_list [{:str "aa" :i  1} {:str "bb" :i  2}]}] (= o (<-message (->message o clz)))))))
