scriptencoding utf-8
let s:save_cpo = &cpo
set cpo&vim

function! s:_SID()
	return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze__SID$')
endfunction


" let s:lambda_template = join(readfile(expand("<sfile>:p:h") . "/reti/lambda_template.txt"), "\n")
let s:lambda_template = join([
\	"function! %s(...)													",
\	"	let Self = function('%s')										",
\	"	if has_key(s:lambda_capture, '%s')								",
\	"		call s:_capture({ 'local' : l: }, s:lambda_capture['%s'])	",
\	"	endif															",
\	"	try																",
\	"		%s															",
\	"	finally															",
\	"		if has_key(s:lambda_capture, '%s') && len(s:lambda_capture['%s']) == 1",
\	"			unlet! Self												",
\	"			call extend(s:lambda_capture['%s'][0], l:)				",
\	"		endif														",
\	"	endtry															",
\	"endfunction														",
\], "\n")


function! s:_gen_lambda(name, expr)
	return printf(s:lambda_template, a:name, a:name, a:name, a:name, a:expr, a:name, a:name, a:name)
endfunction



let s:lambda_counter = 0
let s:lambda_capture = {}
let s:lambda_cache = {}
let s:lambda_expr_cache = {}


function! s:_capture(l, captures)
	for list in a:captures
		call extend(a:l.local, list)
	endfor
endfunction



function! s:execute(expr, ...)
" 	let expr = chained#script_function_to_function_symbol(a:expr, chained#to_SNR(chained#latest_called_script_function()))
	let expr = a:expr
	if has_key(s:lambda_cache, expr) && !a:0
		return s:lambda_cache[expr]
	endif
	let name = "<SNR>" . s:_SID() . "_lambda_".s:lambda_counter
	let name_str = string(name)
	let s:lambda_capture[name] = a:000
	execute s:_gen_lambda(name, expr)
	let s:lambda_counter += 1
	if a:0
		return function(name)
	else
		let s:lambda_cache[expr] = function(name)
		let s:lambda_expr_cache[name] = expr
		return s:lambda_cache[expr]
	endif
endfunction


function! s:eval(expr, ...)
	return call(function("s:execute"), ["return ".a:expr] + a:000)
endfunction


function! s:SCaller(...)
	try
		throw 'abc'
	catch /^abc$/
		if a:0
" 			echo reverse(split(v:throwpoint, '\.\.')[ : -2])
			let prefunc = a:1
			let result = matchstr(v:throwpoint, '^.*\.\.\zs.*\ze\.\.'.prefunc)
			if empty(result)
				let result = matchstr(v:throwpoint, '^function \zs.*\ze\.\.'.prefunc)
			endif
			return result
		else
			return matchstr(v:throwpoint, '^function \zs.\{-}\ze\.\.')
		endif
	endtry
endfunction

function! s:to_script_function(name, prev)
	return substitute(a:name, "s:", matchstr(s:SCaller(a:prev), '\zs<SNR>\d*_\ze.*'), "g")
endfunction


function! s:script(expr)
	return function(s:to_script_function(a:expr, "s:script"))
endfunction


let s:operator_list = [
\	"+", "-", "*", "/", "%", ".",
\	"==", "==#", "==?",
\	"!=", "!=#", "!=?",
\	">",  ">#",  ">?",
\	">=", ">=#", ">=?",
\	"<",  "<#",  "<?",
\	"<=", "<=#", "<=?",
\	"=~", "=~#", "=~?",
\	"!~", "!~#", "!~?",
\	"is", "is#", "is?",
\	"isnot", "isnot#", "isnot?",
\	"||", "&&",
\]

" let s:operator_list = [
" \   '^\s*\(+\)\(.*\)',
" \   '^\s*\(-\)\(.*\)',
" \   '^\s*\(*\)\(.*\)',
" \   '^\s*\(/\)\(.*\)',
" \   '^\s*\(%\)\(.*\)',
" \   '^\s*\(\.\)\(.*\)',
" \   '^\s*\(==#\)\(.*\)',
" \   '^\s*\(==?\)\(.*\)',
" \   '^\s*\(==\)\(.*\)',
" \   '^\s*\(!=#\)\(.*\)',
" \   '^\s*\(!=?\)\(.*\)',
" \   '^\s*\(!=\)\(.*\)',
" \   '^\s*\(>=#\)\(.*\)',
" \   '^\s*\(>=?\)\(.*\)',
" \   '^\s*\(>=\)\(.*\)',
" \   '^\s*\(>#\)\(.*\)',
" \   '^\s*\(>?\)\(.*\)',
" \   '^\s*\(>\)\(.*\)',
" \   '^\s*\(<\)\(.*\)',
" \   '^\s*\(<#\)\(.*\)',
" \   '^\s*\(<?\)\(.*\)',
" \   '^\s*\(<=\)\(.*\)',
" \   '^\s*\(<=#\)\(.*\)',
" \   '^\s*\(<=?\)\(.*\)',
" \   '^\s*\(=\~#\)\(.*\)',
" \   '^\s*\(=\~?\)\(.*\)',
" \   '^\s*\(=\~\)\(.*\)',
" \   '^\s*\(\!\~\)\(.*\)',
" \   '^\s*\(\!\~#\)\(.*\)',
" \   '^\s*\(\!\~?\)\(.*\)',
" \   '^\s*\(is#\)\(.*\)',
" \   '^\s*\(is?\)\(.*\)',
" \   '^\s*\(is\)\(.*\)',
" \   '^\s*\(isnot#\)\(.*\)',
" \   '^\s*\(isnot?\)\(.*\)',
" \   '^\s*\(isnot\)\(.*\)',
" \   '^\s*\(||\)\(.*\)',
" \   '^\s*\(&&\)\(.*\)'
" \]


function! s:is_operator(str)
	return index(s:operator_list, a:str) != -1
endfunction


function! s:to_operator(str)
	let regex = '\(\a\|\d\|_\|""\|''*\)\([+-/*%.=~!?|&#<>]\+\)\(.*\)'
	return substitute(a:str, regex, '\=(empty(submatch(1)) && empty(submatch(3)) ? ("a:1".submatch(2)."a:2") : ((empty(submatch(1)) ? "a:1" : submatch(1)) . submatch(2) . (empty(submatch(3)) ? "a:1" : submatch(3)))) ', "g")
endfunction


function! s:operator(op)
" 	return s:eval(s:to_operator_expr(a:op))
	return s:eval("a:1 ".a:op." a:2")
endfunction


function! s:compose(expr1, expr2, ...)
	let F1 = s:lambda(a:expr1)
	let F2 = a:0 ? call (function("s:compose"), [a:expr2] + a:000) : s:lambda(a:expr2)
	return s:eval("F1(call(F2, a:000))", l:)
endfunction


function! s:function(name, ...)
	try
		if type(a:name) == type(function("tr"))
			return a:name
		endif
		let name = chained#to_function_symbol(a:name, chained#to_SID(chained#latest_called_script_function()))
" 		let name = a:name
		return name =~ '^[a-zA-Z0-9#_:<>]\+$' && exists("*".name) ? function(name) : 0
	catch
		return 0
	endtry
endfunction


function! s:lambda(expr, ...)
	if type(a:expr) == type("") && has_key(s:lambda_cache, a:expr) && !a:0
		return s:lambda_cache[a:expr]
	endif
	let Reti_lambda_func = a:0 ? 0 : s:function(a:expr, "s:lambda")
	return type(Reti_lambda_func) == type(function("tr")) ? Reti_lambda_func
\		 : type(a:expr) == type({}) ? call("s:dict_func", [a:expr] + a:000)
\		 : type(a:expr) == type([]) && len(a:expr) == 1 ? call(function("s:lambda"), a:expr + a:000)
\		 : type(a:expr) == type([]) ? call(function("s:compose"), a:expr)
\		 : s:is_operator(a:expr) ? s:operator(a:expr)
\		 : type(a:expr) == type("") && a:expr[0] ==# ':' ? call(function("s:execute"), [a:expr] + a:000)
\		 : call(function("s:eval"), [a:expr] + a:000)
endfunction


function! s:curry(func)
	return s:lambda('s:lambda("call('.string(a:func).', [".string(a:1)."] + a:000)")')
endfunction


function! s:dict_func(dict, ...)
	let method = get(a:, 1, "apply")
" 	return s:eval("self.".method."()", {"self" : a:dict})
" 	return s:eval("call(self.apply, [], self)", {"self" : a:dict})
	return s:eval("call(self.".method.", a:000, self)", {"self" : a:dict})
endfunction



function! s:max(a, b)
	return a:a > a:b ? a:a : a:b
endfunction


function! s:min(a, b)
	return a:a > a:b ? a:b : a:a
endfunction




function! s:foldl(func, value, seq)
	let Reti_foldl_func = s:lambda(a:func)
	let Result = a:value
	for n in a:seq
		let Result = Reti_foldl_func(Result, n)
	endfor
	return Result
endfunction



function! s:fold(...)
	return call("s:foldl", a:000)
endfunction



function! s:foldr(func, value, seq)
	let Reti_foldr_func = s:lambda(a:func)
	let Result = a:value
	for n in reverse(a:seq)
		let Result = Reti_foldr_func(n, Result)
	endfor
	return Result
endfunction


function! s:default_constructor(type)
	return a:type == type(0)   ? 0
\		 : a:type == type("")  ? ""
\		 : a:type == type([])  ? []
\		 : a:type == type({})  ? {}
\		 : a:type == type(0.0) ? 0.0
\		 : -1
endfunction


function! s:foldl1(func, seq)
	return s:foldl(a:func, get(a:seq, 0), a:seq[1:])
endfunction


function! s:foldr1(func, seq)
	return s:foldr(a:func, get(a:seq, -1), a:seq[0:-2])
endfunction


function! s:map(f, seq)
	let F = s:lambda(a:f)
	return s:fold(s:lambda("add(a:1, F(a:2))", l:), [], a:seq)
" 	let result = []
" 	for n in a:seq
" 		call add(result, a:f(n))
" 	endfor
" 	return result
endfunction


function! s:delete_lambda(f, ...)
	let f = s:as_funcname(a:f)
	if has_key(s:lambda_expr_cache, f)
		unlet! s:lambda_cache[s:lambda_expr_cache[f]]
		unlet! s:lambda_capture[f]
		unlet! s:lambda_expr_cache[f]
	endif
	if get(a:, 1, 1)
		execute printf("delfunction %s", f)
	else
		let name = matchstr(f, '<SNR>\zs.*')
		let map = printf("<Plug>(reti-del-func%s)", name)
		execute "nnoremap <silent>" map printf(":delfunction %s <bar> :nunmap %s<CR>", f, map)
		call feedkeys(printf("\<Plug>(reti-del-func%s)", name))
" 		call feedkeys(printf(":delfunction %s\<CR>", f), "n")
	endif
endfunction


function! s:once(f, ...)
	let F = type(a:f) == type(function("tr")) ? a:f : call("s:lambda", [a:f] + a:000)
	let f = s:as_funcname(F)
	return s:execute(printf("let result = call('%s', a:000) | call s:delete_lambda(function('%s')) | call s:delete_lambda(Self, 0) | return result", f, f))
endfunction


function! s:as_funcname(f)
	return matchstr(string(a:f), 'function(''\zs.\{-}\ze'')')
endfunction


function! s:bind(f, ...)
	let F = type(a:f) == type("") ? s:lambda(a:f) : a:f
	return s:execute(printf("return call('%s', %s + a:000)", s:as_funcname(F), string(a:000)))
endfunction


let &cpo = s:save_cpo
unlet s:save_cpo
