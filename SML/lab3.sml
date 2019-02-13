(* MICHAEL SMITH *)
(* LAB 3 - CS 362*)
(* LAST UPDATED - FEB 4, 2019*)

(* lt subtree, rt subtree, key, value *)
datatype BST = Empty | Node of BST * BST * int * int;

fun parsePost [] = Empty
|   parsePost lst =
    let
        fun pP (stack, (0,k,v)::str) = pP(Node(Empty, Empty, k, v)::stack, str)
        |   pP (L::stack, (1,k,v)::str) = pP(Node(L, Empty, k, v)::stack, str)
        |   pP (R::stack, (2,k,v)::str) = pP(Node(Empty, R, k, v)::stack, str)
        |   pP (R::L::stack, (3,k,v)::str) = pP(Node(L, R, k, v)::stack, str)
        |   pP (T::stack, []) = T;
    in
        pP([],lst)
    end;

val exTree0 = [];
val exTree1 = [(0,1,1),(0,3,3),(3,2,2)];
val exTree2 = [(0,2,2),(2,1,1),(0,4,4),(3,3,3),(0,6,6),(1,7,7),(3,5,5)];
val exTree3 = [(0,1,1),(0,4,4),(1,5,5),(3,2,2),(1,8,8),(0,15,15),(2,14,14),(3,11,11)];

(* INSERTS A NODE INTO THE TREE *)
fun insert (Empty, key, value) = parsePost [(0, key, value)]
  | insert(Node(lt, rt, k, v), key, value) =
    if key < k
      then Node(insert(lt, key, value), rt, k, v)
    else if key > k
      then Node(lt, insert(rt, key, value), k, v)
    else Node(lt, rt, key, value);

(* FINDS A NODE IN THE TREE IF IT EXISTS *)
fun find (Empty, key) = []
  | find (Node(lt, rt, k, v), key) =
    if key < k
      then find(lt, key)
    else if key > k
      then find(rt, key)
    else [v];

(* DELETES A NODE IN THE TREE IF IT EXISTS *)
fun delete (Empty, key) = Empty
  | delete (Node(lt, rt, k, v), key) =
    if key < k then Node(delete(lt, key), rt, k, v)
    else if key > k then Node(lt, delete(rt, key), k, v)
    else
      if rt = Empty
        then lt
      else if lt = Empty
        then rt
      else
        let
          fun minKeyValue(Empty) = []
            | minKeyValue(Node(Empty, Empty, k, v)) = [k, v]
            | minKeyValue(Node(Empty, rt, k, v)) = minKeyValue rt
            | minKeyValue(Node(lt, Empty, k, v)) = minKeyValue lt
            | minKeyValue(Node(lt, rt, k, v)) =
              let val minL = minKeyValue lt
                  val minR = minKeyValue rt
              in if hd minL < hd minR then minL else minR end
        in
          Node(lt, delete(rt, hd (minKeyValue(rt))), hd (minKeyValue(rt)), hd (tl (minKeyValue(rt))))
        end;

(* GETS THE POSTORDER OF THE TREE *)
fun postorder Empty = []
  | postorder (Node(Empty, Empty, k, v)) = [(0,k,v)]
  | postorder (Node(lt, Empty, k, v)) = postorder(lt) @ [(1,k,v)]
  | postorder (Node(Empty, rt, k, v)) = postorder(rt) @ [(2,k,v)]
  | postorder (Node(lt, rt, k, v)) = postorder(lt) @ postorder(rt) @ [(3,k,v)];

(* GENERATES A SUBTREE WITH VALUES GREATER THAN THE MIN AND LESS THAN THE MAX*)
fun subtree (Empty, minKey, maxKey) = Empty
  | subtree (Node(lt, rt, k, v), minKey, maxKey) =
    if k >= minKey andalso k <= maxKey
      then Node(subtree(lt, minKey, maxKey), subtree(rt, minKey, maxKey), k, v)
    else if k < minKey
      then subtree(rt, minKey, maxKey)
    else subtree(lt, minKey, maxKey);
