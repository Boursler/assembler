fn sample:
add rax,rsi

xor rdx,[rcx + (2 * 8 + 4) / 5 * rdx + 16]
mov rsp,8
and

fn jumpsomewhere:
	add rsp, 8
	label: 
		ret

fn dostuff:
	xor rbx,   rbx
label:
		ret
