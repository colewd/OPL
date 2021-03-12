Cole DiStasio
Worked with Erika and Julia Spehlmann

To Compile and Run:
ocamlopt -o a3 translator.ml
ocaml
#use "translator.ml";;
print_string(snd (translate (ast_ize_P (parse ecg_parse_table primes_prog))));;

Used and modified the code provided by professor Wilkes to translate to an AST and from there into C code. 