open Effect
exception Abort_take of string

type 'a mv_state =
  | Full  of 'a * ('a * unit Sched.resumer) Fun_queue.t
  | Empty of 'a Sched.resumer Fun_queue.t

type 'a t = 'a mv_state Atomic.t

let create_empty () = Atomic.make (Empty (Fun_queue.empty))

let create v = Atomic.make (Full (v, Fun_queue.empty))

let sw = ref true 

let rec put v mv =
  let old_contents = Atomic.get (mv) in
  match old_contents with
  | Full (v', q) -> let p = ref true in 
                    perform (Sched.Suspend (fun r -> 
                                            let newQueue = Fun_queue.push q (v,r) in
                                            let new_contents = Full (v', newQueue) in
                                            let var = Atomic.compare_and_set mv old_contents new_contents in
                                            var
                                            ));
                    if !p then
                      ()
                    else
                      put v mv
                    (* TODO: Retry when var is false *)
  | Empty q ->
      if Fun_queue.length q = 0 then 
                  begin
                    let new_contents = Full (v, Fun_queue.empty) in
                    let ret = Atomic.compare_and_set mv old_contents new_contents in 
                    if (not ret) then 
                      put v mv
                  end     
      else
          match Fun_queue.pop q with
                          | None -> ()
                          | Some (x, newQueue) -> let resume = x in
                                                  let new_contents = Empty newQueue in
                                                  let ret = Atomic.compare_and_set mv old_contents new_contents in 
                                                  if ret then
                                                    begin
                                                      let ret1 = resume (Ok v) in
                                                      if ret1 then ()
                                                      else raise (Abort_take "Excception in Put because it is already aborted")  
                                                    end                                               
                                                  else
                                                    put v mv

        

(* TODO -> Check if v is to be returned only on true cases or false cases too!!! *)
let rec take mv =
  let old_contents = Atomic.get mv in 
  match old_contents with
  | Empty q -> let p = ref true in 
                let a = perform (Sched.Suspend (fun r -> 
                                            let newQueue = Fun_queue.push q r in
                                            let new_contents = Empty newQueue in
                                            p := Atomic.compare_and_set mv old_contents new_contents;
                                            !p
                                          )
                        ) in
                        if !p then
                          a
                        else
                          take mv                             
  | Full (v, q) ->
                if Fun_queue.length q = 0 then
                  begin
                    let new_contents = Empty Fun_queue.empty in
                    let ret = Atomic.compare_and_set mv old_contents new_contents in 
                    if ret then 
                      v
                    else 
                      take mv
                  end 
                else
                    match Fun_queue.pop q with
                    | None -> raise (Abort_take "Excception in take when queue popping from empty queue")
                    | Some ((v', resume), newQueue) -> 
                                                  let new_contents = Full (v', newQueue) in
                                                  let ret = Atomic.compare_and_set mv old_contents new_contents in 
                                                  if ret then
                                                    begin
                                                    let ret1 = resume (Ok ()) in 
                                                      if ret1 then v
                                                      else raise (Abort_take "Excception in Take because it is already aborted")   
                                                    end
                                                  else
                                                    take mv               

(* 
lett put v mv = 



else
        let resume = Queue.pop q in
        let ret = resume (Ok v) in
        if ret then ()
        else raise (Abort_take "Excception in Put because it is already aborted")

let take mv =
  match !mv with
  | Empty q -> perform (Sched.Suspend (fun r -> Queue.push r q))
  | Full (v, q) ->
      if Queue.is_empty q then
        (mv := Empty (Queue.create ()); v)
      else begin
        let (v', resume) = Queue.pop q in
        mv := Full (v', q);
        let _ = resume (Ok ()) in ();
        v
      end *)
