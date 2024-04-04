(*                                      
                         CS 51 Problem Set 6
                                Search
 
                             Collections
 *)

(*======================================================================
Before working on this problem set, read the problem set 6 writeup in
the file `readme.pdf`. It provides context and crucial information for
completing the problems. In addition, make sure that you are familiar
with the problem set procedures in the document "Problem set
procedures for CS51".

You are allowed (and encouraged) to work with a partner on this
problem set. You are also allowed to work alone, if you prefer. See
https://cs51.io/procedures/pset-instructions/#working-with-a-partner
for further information on working with partners on problem sets.

IMPORTANT: You should not need and are not intended to use any
imperative programming techniques on this problem set. In particular,
you won't need (and shouldn't use) references, mutability, loops, and
other material from Chapters 15 and 16.
======================================================================*)

(* The `COLLECTION` module signature is a generic data structure
   generalizing stacks, queues, and priority queues, allowing adding
   and taking elements. This file provides the signature and several
   functors implementing specific collections (stacks, queues,
   etc.).
 *)
module type COLLECTION = 
  sig

    (* Empty -- Exception indicates attempt to take from an empty
       collection *)
    exception Empty
                
    (* elements in the collection *)
    type elt
           
    (* collections themselves *)
    type collection
           
    (* empty -- the empty collection, collection with no elements *)
    val empty : collection 
                  
    (* length col -- Returns number of elements in the collection col *)
    val length : collection -> int
                                
    (* is_empty col -- Returns true if and only if the collection col is
       empty *)
    val is_empty : collection -> bool
                                  
    (* add elt col -- Returns a collection like col but with an element
       elt added *)
    val add : elt -> collection -> collection
                                    
    (* take col -- Returns a pair of an element from the collection and
       the collection of the remaining elements; raises Empty if the
       collection is empty. Which element is taken is determined by the
       implementation. *)
    val take : collection -> elt * collection
                                    
  end ;;
  
(*----------------------------------------------------------------------
  Some useful collections

  To think about: For each of these implementations, what is the time
  complexity for adding and taking elements in this kind of
  collection?  *)

(*......................................................................
  Stacks implemented as lists 
 *)
    
module MakeStackList (Element : sig type t end)
       : (COLLECTION with type elt = Element.t) = 
  struct
    exception Empty

    type elt = Element.t
    type collection = elt list

    let empty : collection = []

    let is_empty (d : collection) : bool = 
      d = empty 
            
    let length (d : collection) : int = 
      List.length d
                  
    let add (e : elt) (d : collection) : collection = 
      e :: d
      
    let take (d : collection) :  elt * collection = 
      match d with 
      | hd :: tl -> hd, tl
      | _ -> raise Empty
  end ;;

(*......................................................................
  Queues implemented as lists 
 *)
  
module MakeQueueList (Element : sig type t end)
       : (COLLECTION with type elt = Element.t) =
  struct        
    exception Empty
                
    type elt = Element.t
    type collection = elt list
                          
    let empty : collection = []
                               
    let length (d : collection) : int = 
      List.length d
                  
    let is_empty (d : collection) : bool = 
      d = empty 
            
    let add (e : elt) (d : collection) : collection = 
      d @ [e]
      
    let take (d : collection)  :  elt * collection = 
      match d with 
      | hd :: tl -> hd, tl
      | _ -> raise Empty
  end ;;

(*......................................................................
  Queues implemented as two stacks
  
  In this implementation, the queue is implemented as a pair of stacks
  (s1, s2) where the elements in the queue from highest to lowest
  priority (first to last to be taken) are given by s1 @ s2R (where @
  is the appending function and s2R is the reversal of s2). Elements
  are added (in stack regime) to s2, and taken from s1. When s1 is
  empty, s2 is reversed onto s1. In order to implement the two stacks,
  you might as well use the `MakeStackList` implementation from
  above. See Section 15.5.2 in Chapter 15 for more information on this
  technique. However, unlike the implementation in that section, your
  implementation need not and should not use any imperative
  constructs. *)

  module MakeQueueStack (Element : sig type t end) 
  : (COLLECTION with type elt = Element.t) =
struct
  module Stack = MakeStackList(Element)
  exception Empty

  type elt = Element.t
  type collection = Stack.collection * Stack.collection

  let empty = (Stack.empty, Stack.empty)

  let is_empty (s1, s2) =
  Stack.is_empty s1 && Stack.is_empty s2

  let length (s1, s2) = 
  Stack.length s1 + Stack.length s2

  let add e (s1, s2) =
  (s1, Stack.add e s2)

  let take (s1, s2) =
  match Stack.is_empty s1 with
  | true -> 
      let rec transfer s2_to s1_from =
        if Stack.is_empty s2_to then s1_from, Stack.empty
        else
          let e, s2_new = Stack.take s2_to in
          transfer s2_new (Stack.add e s1_from)
      in
      let s1_new, s2_empty = transfer s2 s1 in
      if Stack.is_empty s1_new then raise Empty
      else let e, s1_rest = Stack.take s1_new in e, (s1_rest, s2_empty)
  | false -> 
      let e, s1_new = Stack.take s1 in
      e, (s1_new, s2)
end ;;


(*======================================================================
Reflection on the problem set

After each problem set, we'll ask you to reflect on your experience.
We care about your responses and will use them to help guide us in
creating and improving future assignments.

........................................................................
Please give us an honest (if approximate) estimate of how long (in
minutes) this problem set (in total, not just this file) took you to
complete. (If you worked with a partner, we're asking for how much time
each of you (on average) spent on the problem set, not in total.)
......................................................................*)

let minutes_spent_on_pset () : int =
  390 ;;

(*......................................................................
It's worth reflecting on the work you did on this problem set. Where
did you run into problems and how did you end up resolving them? What
might you have done in retrospect that would have allowed you to
generate as good a submission in less time? Please provide us your
thoughts on these questions and any other reflections in the string
below.
......................................................................*)

let reflection () : string =
  "On this problem set, I mostly ran into issues with the module `MakePuzzleSolver`, 
  especially getting the search strategies to work right. I solved them by breaking down 
  the problem into smaller parts and trying each piece separately before putting them all together. 
  Also time management and not doing the pset with a partner were difficulties." ;;
