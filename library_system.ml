(* 8.Simple library system *)
type book = {
  title : string;
  author : string;
  year : int;
  is_checked_out : bool;
  genre : string;
}

let available_books books =
  List.filter (fun book -> not book.is_checked_out) books;; (*check if book in library now from value of is_checked_out*)

let group_books_by_genre books =
  let group_by_genre acc book = 
    let genre = book.genre in
    match List.find_opt (fun (g, _) -> g = genre) acc with
    | Some (g, group) -> (g, book :: group) :: List.filter (fun (g, _) -> g <> genre) acc
    | None -> (genre, [book]) :: acc
  in 
  List.fold_left group_by_genre [] books;;

let library = [
  {title = "1984"; author = "George Orwell"; year = 1949; is_checked_out = false; genre = "Dystopian"};
  {title = "The Hobbit"; author = "J.R.R. Tolkien"; year = 1937; is_checked_out = true; genre = "Fantasy"};
  {title = "The Pragmatic Programmer"; author = "Andrew Hunt"; year = 1999; is_checked_out = false; genre = "Programming"};
  {title = "The Lord of the Rings"; author = "J.R.R. Tolkien"; year = 1954; is_checked_out = false; genre = "Fantasy"}
];;

available_books library;;

group_books_by_genre library;;