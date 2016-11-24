(ns grpc.transformer-test
  (:require [clojure.test :refer :all]
            [grpc.transformer :refer :all])
  (:import [michael.test Demo$StringMessage Demo$EnumMessage Demo$RepeatedEnumMessage Demo$IntMessage Demo$RepeatedStringMessage Demo$DemoMessage Demo$Message1 Demo$NestedMessage]))





(deftest test-repeated-string-message
  (testing "test clojure message transfer to protobuf message "
    (let [clz Demo$RepeatedStringMessage
          message-value ["hello" "world"]
          message (->message {:slist message-value} clz)]
      (is (= michael.test.Demo$RepeatedStringMessage (type message)))
      (is (= message-value (.getSlist message))))))



(deftest ->message-test
  (testing "test clojure message transfer to protobuf message "
    (is (nil? (prn     (->message oo clz))))))

(deftest test1
  (testing "he"
    (is (= true
           (do
             (let [clz Demo$StringMessage
                   builder (find-builder clz)
                   descriptor (find-builder-descriptor clz)]
               (prn (->message {:s "aa"} clz)))
             true)))))


(deftest test2
  (testing "he"
    (is (= true
           (do
             (let [clz Demo$RepeatedStringMessage
                   builder (find-builder clz)
                   descriptor (find-builder-descriptor clz)]
               (prn (->message {:slist ["a" "d" "e"]} clz)))
             true)))))


(deftest test3
  (testing "he"
    (is (= true
           (let [clz Demo$EnumMessage
                 builder (find-builder clz)
                 descriptor (find-builder-descriptor clz)]
             (prn (->message {:bar "BAR_B"} clz))
             true)))))



(deftest test4
  (testing "he"
    (is (= true
           (let [clz Demo$RepeatedEnumMessage
                 builder (find-builder clz)
                 descriptor (find-builder-descriptor clz)]
             (prn (->message {:bar ["BAR_B" "BAR_C" "BAR_A" "BAR_B"]} clz))
             true)))))



(deftest test5
  (testing "he"
    (is (= true
           (let [clz Demo$IntMessage
                 builder (find-builder clz)
                 descriptor (find-builder-descriptor clz)]
             (prn (->message {:i 66} clz))
             true)))))




(deftest test6
  (testing "he"
    (is (= true
           (let [clz Demo$Message1
                 builder (find-builder clz)
                 descriptor (find-builder-descriptor clz)]
             (prn (->message {:str "hello world" :i 76} clz))
             true)))))


(deftest test7
  (testing "he"
    (is (= true
           (let [clz Demo$DemoMessage
                 builder (find-builder clz)
                 descriptor (find-builder-descriptor clz)]
             (prn (->message {:m2 {:s "hello world0" :i 1}} clz))
             true)))))



(deftest test8
  (testing "he"
    (is (= true
           (let [clz Demo$DemoMessage
                 builder (find-builder clz)
                 descriptor (find-builder-descriptor clz)]
             (prn (->message {:m2 {:s "hello world0" :i 1}
                              :m_list [{:str "aa" :i  1}
                                       {:str "bb" :i  2}]} clz))
             true)))))


(deftest test9
  (testing "he"
    (is (= true
           (let [clz Demo$NestedMessage
                 builder (find-builder clz)
                 descriptor (find-builder-descriptor clz)]
             (prn (->message {:n {:n2 {:n1 {:str "BBB"} :i {:s "aaaa"}}}} clz))
             true)))))
