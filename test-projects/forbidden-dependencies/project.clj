(defproject deps-plus-tests/forbidden-dependencies "0-SNAPSHOT"
  :managed-dependencies [[io.grpc/grpc-all "1.78.0"]
                         [io.netty/netty-all "4.2.9.Final"]]
  :dependencies [[com.taoensso/nippy "3.6.0"]
                 [io.netty/netty-all]]
  :forbidden-dependencies [com.taoensso/nippy      ; Banned direct dependency
                           io.grpc/grpc-all        ; Banned but not used managed dependency
                           io.netty/netty-common]) ; Banned transitive dependency
