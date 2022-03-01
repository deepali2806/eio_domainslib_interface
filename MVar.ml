(* Taken from https://github.com/kayceesrk/code-snippets/blob/master/scheduler_parateric_mvar/MVar.ml *)

open EffectHandlers

type 'a mv_state =
  | Full  of 'a * ('a * unit Sched.resumer) Queue.t
  | Empty of 'a Sched.resumer Queue.t


type 'a mv_state_mutex = {
  mutex : Mutex.t;
  mv : 'a mv_state ref
}

(* type 'a t = 'a mv_state ref *)

type 'a t = 'a mv_state_mutex

let create_empty () = 
    {
      mutex = Mutex.create ();
      mv = ref (Empty (Queue.create ()))
    }

let create v = 
  {
    mutex = Mutex.create ();
    mv = ref (Full (v, Queue.create ()))
  }

let put v t =
  Mutex.lock t.mutex;
  match !(t.mv) with
  | Full (v', q) -> perform (Sched.Suspend ((fun r -> Queue.push (v,r) q), t.mutex))
  | Empty q ->
      if Queue.is_empty q then
        begin
          t.mv := Full (v, Queue.create ());
          Mutex.unlock t.mutex
        end
      else
        begin
          let resume = Queue.pop q in
          resume v;
          Mutex.unlock t.mutex
        end 

let take t =
  Mutex.lock t.mutex;
  match !(t.mv) with
  | Empty q -> perform (Sched.Suspend ((fun r -> Queue.push r q), t.mutex))
  | Full (v, q) ->
      if Queue.is_empty q then
        begin
            t.mv := Empty (Queue.create ());
            Mutex.unlock t.mutex;
            v
        end
      else 
        begin
          let (v', resume) = Queue.pop q in
          t.mv := Full (v', q);
          resume ();
          Mutex.unlock t.mutex;
          v
        end
      
let check () =
	Printf.printf "\nINSIDE MVAR INTERFACE\n"      

