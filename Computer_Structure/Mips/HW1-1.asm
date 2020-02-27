      .data
n: .word  5
      .text
lb 	$a0 , n
jal	sum
add	$v1, $v1, $v0

exit:
li	$v0, 10
syscall

#put your code here
sum:
addi    $sp, $sp, -8
sw	$ra, 0($sp)
sw	$a0, 4($sp)

slti    $t0, $a0, 1
beq	$t0, $0, recur

add	$v0, $0, $0
addi    $sp, $sp, 8
jr	$ra

recur:
addi    $a0, $a0, -1
jal	sum

lw	$ra, 0($sp)
lw	$a0, 4($sp)
addi    $sp, $sp, 8

add	$v0, $a0, $v0
jr	$ra

