(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %mcqc H3.json -o %t.cpp
    RUN: %FC %s -check-prefix=CPP < %t.cpp
    RUN: %clang -c %t.cpp

    CPP: #include "nat.hpp"
    CPP: bool isEven(nat n)
    CPP: return match(n,
    CPP: return true
    CPP: return match(sm,
    CPP: return false
    CPP: return isEven(m)

    CPP: template<{{typename|class}} [[TF:.?]]>
    CPP: std::shared_ptr<list<nat>> mapOnEvensM([[TF]] f, nat n, std::shared_ptr<list<nat>> l)
    CPP: return match(l,
    CPP: return coq_nil<nat>()
    CPP: return match(isEven(n),
    CPP: return coq_cons<nat>(f(h), mapOnEvensM(f, {{.*}}, ts
    CPP: return coq_cons<nat>(h, mapOnEvensM(f, {{.*}}, ts

    CPP: template<{{typename|class}} [[TF:.?]]>
    CPP: std::shared_ptr<list<nat>> mapOnEvens([[TF]] f, std::shared_ptr<list<nat>> l)
    CPP: len = length(l)
    CPP: return match(len,
    CPP: return coq_nil<nat>()
    CPP: return mapOnEvensM(f, n, l)
*)
Add Rec LoadPath "../../classes" as Mcqc.
From Mcqc Require MIO.
Import MIO.IO.
From Mcqc Require MShow.
Import MShow.Show.
Require Export Coq.Lists.List.
Import ListNotations.

Fixpoint isEven(n: nat) :=
  match n with
    | 0 => true
    | 1 => false
    | S(S m as sm) => isEven m
  end.

Fixpoint mapOnEvensM (f : nat -> nat) (n: nat) (l: list nat) : list nat :=
  match l with
    | [] => []
    | h::ts =>
      match isEven n with
      | true => (f h) :: (mapOnEvensM f (n-1) ts)
      | false => h :: (mapOnEvensM f (n-1) ts)
      end
  end.

Definition mapOnEvens (f : nat -> nat) (l : list nat) : list nat :=
  let len := length l in
  match len with
    | 0 => []
    | S n => mapOnEvensM f n l
  end.

Require Extraction.
Extraction Language JSON.
Separate Extraction mapOnEvens.
