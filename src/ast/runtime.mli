type proc_t = { f : string; ps : string list; c : Program.cmd }

val to_proc_t : Triple.t -> proc_t
val exec : proc_t list -> int
