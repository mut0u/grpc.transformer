(defproject grpc.transformer "0.1.0-SNAPSHOT"
  :description "This is the grpc message transformer for clojure"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [io.grpc/grpc-protobuf "1.4.0"]
                 [io.grpc/grpc-stub "1.4.0"]
                 [io.grpc/grpc-core "1.4.0"]]
  :main ^:skip-aot grpc.transformer
  :target-path "target/%s"
  :java-source-paths ["proto/build/generated/source/proto/main/grpc"
                      "proto/build/generated/source/proto/main/java"]
  :profiles {:uberjar {:aot :all}})
