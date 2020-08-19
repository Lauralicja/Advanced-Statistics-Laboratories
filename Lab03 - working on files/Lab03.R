# Exercise 1.
create_lab_directory <- function(name){
  x = getwd()
  dir.create(name)
}
# Exercise 2.
create_file <- function(name){
  file.create(name)
}

pick_files <- function(){
  Names = choose.files()
  return (Names)
}

check_existance <- function(Names){
  if (!file.exists(Names)){
    print("File does not exist.")
  }
  else {
    print("File exists.")
  }
}

remove_file <- function(name){
  file.remove(name)
}

# Exercise 3.
copy_and_append <- function(filename1, filename2){
  file.copy(filename1, filename2)
  file.append(filename2, filename1)
  res <- read.csv(filename2, sep=";")
  View(res)
}
