(*4.Find word in the text*)
type line=Newline | Word of string*line and text=End | Line of line*text;;
let locate_string goal txt=
  let rec search_line word line_found=function
    | Newline->line_found (*If we reached end of the string, return line_found (if we have found word)*)
    | Word (w, rest)->search_line word (line_found||w=word) rest (*Take first word and check this word, if w = word update line_found and rec call search_line for another*)
  in
  let rec search_text idx acc=function (*Find word in all parts of the text*)
    | End->List.rev acc (*If we reached end*)
    | Line (l, rest)-> 
        let found=search_line goal false l (*Call for find words*)
        in
        let new_acc=if found then idx::acc else acc (*If found=true, add idx in acc or not to do something*)
        in
        search_text (idx + 1) new_acc rest (*Recursiv call search_text and increase idx for next strng*)
  in
  search_text 0 [] txt;;
let poem =
  Line (Word ("My", Word ("Captain", Word ("does", Word ("not", Word ("answer", Word (",", Word ("his", Word ("lips", Word ("are", Word ("pale", Word ("and", Word ("still", Newline)))))))))))),  
        Line (Word ("My", Word ("father", Word ("does", Word ("not", Word ("feel", Word ("my", Word ("arm", Word (",", Word ("he", Word ("has", Word ("no", Word ("pulse", Word ("nor", Word ("will", Newline)))))))))))))), 
              Line (Word ("The", Word ("ship", Word ("is", Word ("anchor’d", Word ("safe", Word ("and", Word ("sound", Word (",", Word ("its", Word ("voyage", Word ("closed", Word ("and", Word ("done", Newline))))))))))))),  
                    Line (Word ("From", Word ("fearful", Word ("trip", Word ("the", Word ("victor", Word ("ship", Word ("comes", Word ("in", Word ("with", Word ("object", Word ("won", Newline))))))))))),  
                          End))));;
locate_string "and" poem;;
locate_string "the" poem;;
locate_string "," poem;; 