(defproject deps-plus-tests/classpath-conflict "0-SNAPSHOT"
  :dependencies [[io.grpc/grpc-netty-shaded "1.20.0"]
                 ;; Included to ensure we also have unshaded copies of Netty
                 ;; dependencies to search. We do not want who-shades to return
                 ;; self matches (e.g. netty-common does not shade netty-common).
                 [io.grpc/grpc-netty "1.20.0"]
                 [io.honeycomb.libhoney/libhoney-java "1.3.1"]])
