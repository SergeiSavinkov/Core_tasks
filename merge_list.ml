(*3.Merge sorted lists of pairs*)
type ('a,'b) listofpairs=Empty | Nonempty of 'a*'b*('a,'b) listofpairs;;
let rec merge_2_lop lop1 lop2=
  match lop1,lop2 with
  | Empty, lst | lst, Empty -> lst (*If one list empty return another*)
  | Nonempty (k1,v1,t1), Nonempty (k2,v2,t2) -> (*key, value, tail*)
      if k1<k2 then Nonempty (k1,v1,merge_2_lop t1 lop2)
      else Nonempty (k2,v2, merge_2_lop t2 lop1);;
let l1= Nonempty ("Donald Trump", 10,
                  Nonempty ("Janez Novak ", 1, 
                            Nonempty ("Keanu Reeves", -4, Empty)));;
let l2=Nonempty ("Donald Duck", 5, 
                 Nonempty ("France Preseren", 1,
                           Nonempty ("Jana Voda", 2, 
                                     Nonempty ("Kelly Clarkson", 4, Empty))));;
let l3=Nonempty ("Doner Kebab", 7, 
                 Nonempty ("Jaslice", -1, Empty));;
merge_2_lop l1 l2;;
merge_2_lop l3 l2;;
merge_2_lop l1 l3;;
merge_2_lop l1 l2 |> merge_2_lop l3;;