type year = int

type month = int

type day = int

type date = year * month * day

type hour = int

type minute = int

type second = int

type time = hour * minute * second

type datetime = date * time

external local_time : unit -> datetime = ""

external universal_time : unit -> datetime = ""
