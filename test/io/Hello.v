(**
  RUN: %coqc %s
  RUN: %clean
  RUN: %mcqc Hello.json -o %t.cpp
  RUN: %FC %s -check-prefix=CPP < %t.cpp
  RUN: %clang %t.cpp

  CPP: #include "io.hpp"
  CPP: using namespace Io;
  CPP: int main()
  CPP: print(string("Hello world"));
  CPP: return 0;
*)
Add Rec LoadPath "../../classes" as Mcqc.
From Mcqc Require MIO.
Import MIO.IO.

Require Import Coq.Strings.String.
Local Open Scope string_scope.

Definition main :=
  print("Hello world").

Require Extraction.
Extraction Language JSON.
Separate Extraction main.
