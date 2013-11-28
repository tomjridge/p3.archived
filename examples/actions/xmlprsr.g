START -> STATE                                                   { fun i -> let _ = print_string "Parsed a state\n" in i }

STATE -> "<state>" ?epsws? TIME ?epsws? PROTOS ?epsws? OBJS ?epsws? RLNS ?epsws? "</state>"         
                                                                 { fun (_,(_,(t,(_,(ps,(_,(os,(_,(rlns,(_,_)))))))))) -> (t,ps,os,rlns) }

TIME -> "<time>" ?num? "</time>"                                 { fun (_,(n,_)) -> content n }

PROTOS -> "<protos>" ?epsws? OBJLIST ?epsws? "</protos>"               
                                                                 { fun (_,(_,(os,(_,_)))) -> os }

OBJLIST -> ""                                                    { fun _ -> [] }
  | OBJ ?epsws? OBJLIST                                          { fun (f,(_,l)) -> f::l }

OBJ -> "<obj>" ?epsws? CLASS ?epsws? ID ?epsws? FIELDS ?epsws? "</obj>"      
                                                                 { fun (_,(_,(c,(_,(i,(_,(fs,(_,_)))))))) -> (c,i,fs) } 

OBJS -> "<objs>" ?epsws? OBJLIST ?epsws? "</objs>"               { fun (_,(_,(os,(_,_)))) -> os }

CLASS -> "<clazz>" ?notltgt? "</clazz>"                          { fun (_,(c,_)) -> content c }

ID -> "<id>" ?notltgt? "</id>"                                   { fun (_,(i,_)) -> content i }

FIELDS -> "<fields>" ?epsws? FIELDLIST ?epsws? "</fields>"       { fun (_,(_,(fs,(_,_)))) -> fs }

FIELDLIST -> ""                                                  { fun _ -> [] } 
  | FIELD FIELDLIST                                              { fun (f,l) -> f::l }

FIELD -> "<key>" ?notltgt? "</key><value>" ?notltgt? "</value>"  { fun (_,(k,(_,(v,_)))) -> (content k,content v) }

RLNS -> "<rlns>" ?epsws? RLNLIST ?epsws? "</rlns>"               { fun (_,(_,(rs,(_,_)))) -> rs }

RLNLIST -> ""                                                    { fun _ -> [] }
  | RLN ?epsws? RLNLIST                                          { fun (r,(_,rs)) -> r::rs }

RLN -> INV                                                       { fun i -> Inl i }
  | RELN                                                         { fun i -> Inr i }

INV -> "<Inv>" PAIR "</Inv>"                                     { fun (_,(s12,_)) -> s12 }

PAIR -> "<s1>" ?notltgt? "</s1><s2>" ?notltgt? "</s2>"           { fun (_,(s1,(_,(s2,_)))) -> (content s1,content s2) }

RELN -> "<Reln><reln>" ?epsws? NAME ?epsws? SRC ?epsws? DST ?epsws? PAIRS ?epsws? "</reln></Reln>"
                                                                 { fun (_,(_,(n,(_,(s,(_,(d,(_,(ps,(_,_)))))))))) -> (n,s,d,ps) }

NAME -> "<name>" ?notltgt? "</name>"                             { fun (_,(n,_)) -> content n }

SRC -> "<src>" ?notltgt? "</src>"                                { fun (_,(n,_)) -> content n }

DST -> "<dst>" ?notltgt? "</dst>"                                { fun (_,(n,_)) -> content n }

PAIRS -> "<pairs>" PAIRLIST "</pairs>"                           { fun (_,(ps,_)) -> ps }

PAIRLIST -> ""                                                   { fun _ -> [] }
  | PAIR PAIRLIST                                                { fun (p,ps) -> p::ps }


COMMENT -> "for the generated code, we really do want a separate hashtable for each nonterminal; PROTOS is ambiguous if protolist is empty, but result is not - just take head of results; probably parser doesn't like {} in actions" { fun _ -> "" }
