	.data
n: .word 5
	.text
lb	$a0,	n
jal	fib
addi	$v1,	$v0,	0

exit:
li 	$v0,	10
syscall

#put your code here
fib:
addi    $sp, $sp, -8
sw	$ra, 0($sp)
sw	$a0, 4($sp)
addi	$t2, $a0, 0 
addi	$t0, $0, 1

beq	$a0, $t0, one
slti	$t1, $a0 2
beq	$t1, $0 recur
jr	$ra

recur:
addi    $a0, $t2, -1
jal	fib

lw	$a0, 4($sp)
addi	$t2, $a0, 0 

addi	$a0, $t2, -2
jal	fib

lw	$ra, 0($sp)
addi    $sp, $sp, 8
jr	$ra

one:
lw	$ra, 0($sp)
addi	$v0, $v0, 1
jr	$ra
