(defproject deps-plus-tests/family-mismatch "0-SNAPSHOT"
  ;; grpc-core 1.20 depends on gson 2.7
  :managed-dependencies [[com.google.code.gson/gson "2.13.2"]]
  :dependencies [[io.grpc/grpc-core "1.78.0"]])
