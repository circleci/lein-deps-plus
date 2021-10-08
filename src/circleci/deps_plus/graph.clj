(ns circleci.deps-plus.graph)

(defn graph-path-seq
  "Returns a sequence of non-cyclic paths found via a breadth-first walk.
  Equivalent to `graph-seq`, but each node is returned as a vector of nodes
  represent the path from `root` (the first element) to the visited node (the last element).
  `neighbors` must be a fn of one arg that returns the nodes adjacent to the given node.
  `root` is the starting node."
  [neighbors root]
  (loop [to-visit [[root]] visited []]
    (if (seq to-visit)
      (let [path (first to-visit)
            path-set (into #{} path)
            adj (->> (neighbors (peek path))
                     (remove path-set))
            adj-paths (map #(conj path %) adj)]
        (recur
          (into (subvec to-visit 1) adj-paths)
          (conj visited path)))
      (seq visited))))

(defn graph-seq
  "Returns a sequence of graph nodes found via a breadth-first walk. Inspired by `tree-seq`.
  `neighbors` must be a fn of one arg that returns the nodes adjacent to the given node.
  `root` is the starting node."
  [neighbors root]
  (->> (graph-path-seq neighbors root)
       (map peek)
       (distinct)))
