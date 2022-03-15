open Effect

(* module Fun_queue = Fun_queue.t *)

type 'a mv_state =
  | Full  of 'a * ('a * unit Sched.resumer) Fun_queue.t
  | Empty of 'a Sched.resumer Fun_queue.t

(* type 'a t = 'a mv_state ref Atomic.t *)

type 'a t = 'a mv_state Atomic.t 

(* let create_empty () = ref (Empty (Ws_deque.create ())) *)
let create_empty () = Atomic.make (Empty (Fun_queue.empty))
(* let create v = ref (Full (v, Ws_deque.create ())) *)
let create v = Atomic.make (Full (v, Fun_queue.empty))


let put v mv =
  let old_contents = Atomic.get (mv) in
  match old_contents with
  | Full (v', q) -> let ret1 = perform (Sched.Suspend (fun r -> 
                                            let newQueue = Fun_queue.push q (v,r) in
                                            let new_contents = Full (v', newQueue) in
                                            let ret = Atomic.compare_and_set mv old_contents new_contents in
                                            ret
                                            (* if ret then true
                                            else false *)
                                          )
                            ) in
                    if ret1 then
                      Printf.printf "Suspend True"
                    else Printf.printf "Suspend False Retry" 
  | Empty q -> if Fun_queue.length q = 0 then 
                  begin
                    let new_contents = Full (v, Fun_queue.empty) in
                    let ret = Atomic.compare_and_set mv old_contents new_contents in 
                    if ret then 
                      Printf.printf "Empty  Empty True"
                    else Printf.printf "Empty Empty False Retry"   
                  end            
              else
                  (* begin *)
                    let _ = 
                          (match Fun_queue.pop q with
                          | None -> ()
                          | Some (x, newQueue) -> (let resume = x in
                                                  let new_contents = Empty newQueue in
                                                  let ret = Atomic.compare_and_set mv old_contents new_contents in 
                                                  if ret then
                                                    begin
                                                    resume v;
                                                    Printf.printf "Empty resume true"
                                                    end
                                                  else
                                                    Printf.printf "Empty resume false Retry") )
                                                    in ()
                                                        


(* TODO -> Check if v is to be returned only on true cases or false cases too!!! *)
let take mv =
  let old_contents = Atomic.get mv in 
  match old_contents with
  | Empty q -> let ret = perform (Sched.Suspend (fun r -> 
                                            let newQueue = Fun_queue.push q r in
                                            let new_contents = Empty newQueue in
                                            let ret = Atomic.compare_and_set mv old_contents new_contents in
                                            if ret then true
                                            else false
                                          )
                            ) in
                    if ret then
                      Printf.printf "Take Suspend True"
                    else Printf.printf "Take Suspend False Retry" 
  | Full (v, q) ->
                if Fun_queue.length q = 0 then
                  begin
                    let new_contents = Empty Fun_queue.empty in
                    let ret = Atomic.compare_and_set mv old_contents new_contents in 
                    (if ret then 
                      Printf.printf "Take Full  Empty True"
                    else Printf.printf "Take Full Empty False Retry" 
                    );
                    v
                  end 
                else
                      let _ = 
                          (match Fun_queue.pop q with
                          | None -> ()
                          | Some ((v', resume), newQueue) -> 
                                                  (let new_contents = Full (v', newQueue) in
                                                  let ret = Atomic.compare_and_set mv old_contents new_contents in 
                                                  if ret then
                                                    begin
                                                    resume ();
                                                    Printf.printf "Empty resume true";
                                                    v
                                                    end
                                                  else
                                                    Printf.printf "Empty resume false Retry";v) ) in ()
                