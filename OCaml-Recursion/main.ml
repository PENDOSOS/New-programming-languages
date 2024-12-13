type 'a bintree =
  | Leaf
  | Node of 'a bintree * 'a * 'a bintree

let rec sum_tree tree =
  match tree with
  | Leaf -> 0
  | Node(left, value, right) -> value + sum_tree left + sum_tree right

(* Пример использования *)
let tree = Node(Node(Node(Leaf, 21, Leaf), 2, Leaf), 5, Node(Leaf, 3, Leaf))
let result = sum_tree tree
let () = Printf.printf "Сумма всех значений узлов: %d\n" result