{module, tiny}.  %% version = 0

{exports, [{loop,2},{module_info,0},{module_info,1},{start,1}]}.

{attributes, []}.

{labels, 11}.


{function, loop, 2, 2}.
  {label,1}.
    {line,[{location,"tiny.erl",7}]}.
    {func_info,{atom,tiny},{atom,loop},2}.
  {label,2}.
    {allocate_heap,2,2,2}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{literal,<<"~p\n">>},{x,0}}.
    {line,[{location,"tiny.erl",8}]}.
    {call_ext,2,{extfunc,io,format,2}}.
    {move,{y,1},{x,0}}.
    {line,[{location,"tiny.erl",9}]}.
    {call_ext,1,{extfunc,timer,sleep,1}}.
    {line,[{location,"tiny.erl",10}]}.
    {gc_bif,'*',{f,0},0,[{y,1},{integer,2}],{x,0}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,2},2}.


{function, start, 1, 4}.
  {label,3}.
    {line,[{location,"tiny.erl",12}]}.
    {func_info,{atom,tiny},{atom,start},1}.
  {label,4}.
    {make_fun2,{f,10},0,0,1}.
    {'%',{var_info,{x,0},[{fun_type,none}]}}.
    {line,[{location,"tiny.erl",12}]}.
    {call_ext_only,1,{extfunc,process,make,1}}.


{function, module_info, 0, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,tiny},{atom,module_info},0}.
  {label,6}.
    {move,{atom,tiny},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 8}.
  {label,7}.
    {line,[]}.
    {func_info,{atom,tiny},{atom,module_info},1}.
  {label,8}.
    {move,{x,0},{x,1}}.
    {move,{atom,tiny},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-start/1-fun-0-', 3, 10}.
  {label,9}.
    {line,[{location,"tiny.erl",12}]}.
    {func_info,{atom,tiny},{atom,'-start/1-fun-0-'},3}.
  {label,10}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,2}}.
