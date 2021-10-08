(ns circleci.deps-plus.graph-test
  (:require [clojure.test :refer [deftest is testing]])
  (:require [circleci.deps-plus.graph :as graph]))

(defn- neighbors-fn [g]
  (fn [n] (sort (get g n))))

(deftest graph-seq
  (testing "break cycle"
    (let [g {:a #{:b}
             :b #{:a}}]
      (is (= [:a :b]
            (into [] (graph/graph-seq (neighbors-fn g) :a))))))

  (testing "breadth first"
    (let [g {:a #{:b :c}
             :b #{:d :e}
             :c #{:f :g}}]
      (is (= [:a :b :c :d :e :f :g]
            (into [] (graph/graph-seq (neighbors-fn g) :a))))))

  (testing "remove duplicates"
    (let [g {:a #{:b :c}
             :b #{:c}}]
      (is (= [:a :b :c]
            (into [] (graph/graph-seq (neighbors-fn g) :a)))))))

(deftest graph-path-seq
  (testing "break cycle"
    (let [g {:a #{:b}
             :b #{:c}
             :c #{:b}}]
      (is (= [[:a]
              [:a :b]
              [:a :b :c]]
             (into [] (graph/graph-path-seq (neighbors-fn g) :a))))))

  (testing "breadth first"
    (let [g {:a #{:b :c}
             :b #{:d :e}
             :c #{:f :g}}]
      (is (= [[:a]
              [:a :b]
              [:a :c]
              [:a :b :d]
              [:a :b :e]
              [:a :c :f]
              [:a :c :g]]
             (into [] (graph/graph-path-seq (neighbors-fn g) :a))))))

  (testing "follow all paths"
    (let [g {:a #{:b :c}
             :b #{:c}}]
      (is (= [[:a]
              [:a :b]
              [:a :c]
              [:a :b :c]]
            (into [] (graph/graph-path-seq (neighbors-fn g) :a)))))))
