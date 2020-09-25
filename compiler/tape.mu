Opcode enum {
	push
	pushArray
	load
	store
	mask
	index
	shlOr
	jumpIfZero
	jump
	nop
	dup
	discard
	neg
	invert
	add
	sub
	and
	or
	xor
	mul
	eq
	neq
	lt
	lte
	gt
	gte
	shl
	shr
	slice
	storeSlice
	swizzle

	toString(op Opcode) {
		if op == Opcode.push {
			return "push"
		} else if op == Opcode.pushArray {
			return "pushArray"
		} else if op == Opcode.load {
			return "load"
		} else if op == Opcode.store {
			return "store"
		} else if op == Opcode.mask {
			return "mask"
		} else if op == Opcode.index {
			return "index"
		} else if op == Opcode.shlOr {
			return "shlOr"
		} else if op == Opcode.jumpIfZero {
			return "jumpIfZero"
		} else if op == Opcode.jump {
			return "jump"
		} else if op == Opcode.nop {
			return "nop"
		} else if op == Opcode.dup {
			return "dup"
		} else if op == Opcode.discard {
			return "discard"
		} else if op == Opcode.neg {
			return "neg"
		} else if op == Opcode.invert {
			return "invert"
		} else if op == Opcode.add {
			return "add"
		} else if op == Opcode.sub {
			return "sub"
		} else if op == Opcode.and {
			return "and"
		} else if op == Opcode.or {
			return "or"
		} else if op == Opcode.xor {
			return "xor"
		} else if op == Opcode.mul {
			return "mul"
		} else if op == Opcode.eq {
			return "eq"
		} else if op == Opcode.neq {
			return "neq"
		} else if op == Opcode.lt {
			return "lt"
		} else if op == Opcode.lte {
			return "lte"
		} else if op == Opcode.gt {
			return "gt"
		} else if op == Opcode.gte {
			return "gte"
		} else if op == Opcode.shl {
			return "shl"
		} else if op == Opcode.shr {
			return "shr"
		} else if op == Opcode.slice {
			return "slice"
		} else if op == Opcode.storeSlice {
			return "storeSlice"
		} else if op == Opcode.swizzle {
			return "swizzle"
		} else {
			return "?"
		}
	}
}

Instruction struct {
	op Opcode
	z ulong
}

EmulatorRunner {
	run(s EmulatorState, program List<Instruction>) {
		stack := s.stack
		assert(stack.count == 0)
		pc := 0
		while pc < program.count {
			ins := program[pc]			
			top := stack.count - 1
			lhs := stack.count - 2
			if ins.op == Opcode.dup {
				stack.add(stack[top])
			} else if ins.op == Opcode.discard {
				stack.setCountChecked(top)
			} else if ins.op == Opcode.push {
				stack.add(Value { kind: ValueKind.ulong_, z: ins.z })
			} else if ins.op == Opcode.pushArray {
				stack.add(Value { kind: ValueKind.byteArray, z: ins.z })
			} else if ins.op == Opcode.load {
				stack.add(s.rs[cast(ins.z, uint)])
			} else if ins.op == Opcode.store {				
				setSlot(s, cast(ins.z, int), stack[top])
				stack.setCountChecked(top)
			} else if ins.op == Opcode.mask {
				assert(stack[top].kind == ValueKind.ulong_)
				stack[top].z &= ins.z
			} else if ins.op == Opcode.index {
				shr := ins.z >> 32
				mask := (1_uL << (ins.z & 0xffffffff_uL)) - 1
				stack[top].z = (unpack(stack[top]) >> shr) & mask
			} else if ins.op == Opcode.shlOr {
				binaryOperator(s, stack, (unpack(stack[lhs]) << ins.z) | unpack(stack[top]))
			} else if ins.op == Opcode.jumpIfZero {
				if stack[top].z == 0 {
					pc += cast(ins.z, int)
				}
				stack.setCountChecked(top)
			} else if ins.op == Opcode.jump {
				pc += cast(ins.z, int)
			} else if ins.op == Opcode.neg {
				stack[top].z = -unpack(stack[top])
			} else if ins.op == Opcode.invert {
				stack[top].z = ~unpack(stack[top])
			} else if ins.op == Opcode.add {
				binaryOperator(s, stack, unpack(stack[lhs]) + unpack(stack[top]))
			} else if ins.op == Opcode.sub {
				binaryOperator(s, stack, unpack(stack[lhs]) - unpack(stack[top]))
			} else if ins.op == Opcode.and {
				binaryOperator(s, stack, unpack(stack[lhs]) & unpack(stack[top]))
			} else if ins.op == Opcode.or {
				binaryOperator(s, stack, unpack(stack[lhs]) | unpack(stack[top]))
			} else if ins.op == Opcode.xor {
				binaryOperator(s, stack, xor(unpack(stack[lhs]), unpack(stack[top])))
			} else if ins.op == Opcode.mul {
				binaryOperator(s, stack, unpack(stack[lhs]) * unpack(stack[top]))
			} else if ins.op == Opcode.eq {
				binaryOperator(s, stack, unpack(stack[lhs]) == unpack(stack[top]) ? 1_uL : 0)
			} else if ins.op == Opcode.neq {
				binaryOperator(s, stack, unpack(stack[lhs]) != unpack(stack[top]) ? 1_uL : 0)
			} else if ins.op == Opcode.lte {
				binaryOperator(s, stack, unpack(stack[lhs]) <= unpack(stack[top]) ? 1_uL : 0)
			} else if ins.op == Opcode.lt {
				binaryOperator(s, stack, unpack(stack[lhs]) < unpack(stack[top]) ? 1_uL : 0)
			} else if ins.op == Opcode.gte {
				binaryOperator(s, stack, unpack(stack[lhs]) >= unpack(stack[top]) ? 1_uL : 0)
			} else if ins.op == Opcode.gt {
				binaryOperator(s, stack, unpack(stack[lhs]) > unpack(stack[top]) ? 1_uL : 0)
			} else if ins.op == Opcode.shl {
				binaryOperator(s, stack, unpack(stack[lhs]) << unpack(stack[top]))
			} else if ins.op == Opcode.shr {
				binaryOperator(s, stack, unpack(stack[lhs]) >> unpack(stack[top]))
			} else if ins.op == Opcode.slice {
				value := slice(s, stack[stack.count - 3], ins.z, unpack(stack[lhs]), unpack(stack[top]))
				stack.setCountChecked(stack.count - 3)
				stack.add(value)
			} else if ins.op == Opcode.storeSlice {
				storeSlice(s, cast(ins.z, int), unpack(stack[stack.count - 3]), unpack(stack[lhs]), stack[top])
				stack.setCountChecked(stack.count - 3)
			} else if ins.op == Opcode.swizzle {
				value := swizzle(s, stack[stack.count - 5], cast(unpack(stack[stack.count - 4]), int), cast(unpack(stack[stack.count - 3]), int), cast(unpack(stack[lhs]), int), cast(unpack(stack[top]), int))
				stack.setCountChecked(stack.count - 5)
				stack.add(value)
			} else {
				abandon()
			}
			pc += 1
		}
		assert(stack.count == 0)
	}

	binaryOperator(s EmulatorState, stack List<Value>, result ulong) {
		stack[stack.count - 2].z = result
		stack.setCountChecked(stack.count - 1)
	}

	setInput(s EmulatorState, inst ModuleInstance, input string, val ulong) {
		si := inst.localState[inst.def.symbols.get(input).as(ModuleInputDef).localId]
		setSlot(s, si, Value { kind: ValueKind.ulong_, z: val })
		s.started[si] = true
		s.done[si] = true
	}

	setSlot(s EmulatorState, si int, val Value) {
		if si < s.ws.count {
			dest := ref s.ws[si]
			if dest.kind == ValueKind.ulong_ {
				dest.z = unpackAsULong(val)
			} else if dest.kind == ValueKind.byteArray {
				copyToArray(val, dest)
			} else {
				abandon()
			}
		} else {
			assert(val.kind == ValueKind.ulong_ || val.kind == ValueKind.byteArray)
			s.rs[si] = val
		}
	}

	slice(s EmulatorState, target Value, targetWidth ulong, offset ulong, width ulong) {
		if width > 64 {
			return bigSlice(s, target, targetWidth, offset, width)
		}
		data := unpackAsArray(ref target)
		dataWidth := min(checked_cast(data.count, ulong) * 8, targetWidth)
		result := Value { kind: ValueKind.ulong_ }
		to := offset
		offset = min(offset + width, dataWidth)
		while offset > to {
			offset -= 1
			result.z <<= 1
			index := cast(offset >> 3, uint)
			mask := 1 << (offset & 7)
			if data[index] & mask != 0 {
				result.z |= 1
			}
		}
		return result
	}

	storeSlice(s EmulatorState, si int, offset ulong, width ulong, val Value) {	
		dest := ref s.ws[si]
		data := unpackAsArray(dest)
		z := unpackAsULong(val)
		dataWidth := checked_cast(min(data.count * 8, TypeChecker.unpackWidth(s.infos[si].tag)), ulong)
		to := min(offset + width, dataWidth)
		while offset < to {
			index := cast(offset >> 3, uint)
			mask := 1 << (offset & 7)
			if z & 1 == 1 {
				data[index] = cast(data[index] | mask, byte)
			} else {
				data[index] = cast(data[index] & ~mask, byte)
			}
			offset += 1
			z >>= 1
		}
	}

	bigSlice(s EmulatorState, target Value, targetWidth ulong, offset ulong, width ulong) {
		data := unpackAsArray(ref target)
		size := cast(data.count, ulong) * 8
		from := min(offset, size)
		to := min(offset + width, size)
		fromByte := cast(from / 8, int)
		toByte := cast((to + 7) / 8, int)		
		fromBit := from % 8
		toBit := to % 8
		if fromBit == 0 && toBit == 0 && target.kind == ValueKind.byteArray {
			return packArray(new unpackArray(target).slice(fromByte, toByte))
		}
		count := toByte - fromByte
		result := new Array<byte>(count)
		data.copySlice(0, count, result, 0)
		Util.shlArray(result, cast(fromBit, int))
		return packArray(result)
	}

	swizzle(s EmulatorState, target Value, targetWidth int, seqSize int, readStep int, blockSize int) {
		data := unpackAsArray(ref target)
		result := new Array<byte>(targetWidth / 8)
		blocks := targetWidth / blockSize
		cycles := readStep / seqSize
		stepsPerCycle := blockSize / readStep

		w := 0		
		for b := 0; b < blocks {
			for i := 0; i < cycles {
				for j := 0; j < stepsPerCycle {
					r := b * blockSize + i * seqSize + j * readStep
					for k := 0; k < seqSize {
						readIndex := r >> 3
						readByte := readIndex < data.count ? data[readIndex] : 0
						readMask := 1 << (r & 7)
						if readByte & readMask != 0 {
							writeMask := 1 << (w & 7)
							result[w>> 3] = cast(result[w >> 3] | writeMask, byte)
						}				
						r += 1
						w += 1
					}
				}
			}
		}

		assert(w == targetWidth)
		
		return targetWidth > 64 ? EmulatorRunner.packArray(result) : EmulatorRunner.packULong(result)
	}

	unpack(v Value) {
		assert(v.kind == ValueKind.ulong_)
		return v.z
	}

	unpackArray(v Value) {
		assert(v.kind == ValueKind.byteArray)
		return transmute(v.z, Array<byte>)
	}

	unpackAsULong(v Value) {
		if v.kind == ValueKind.ulong_ {
			return v.z
		} else if v.kind == ValueKind.byteArray {
			z := 0_uL
			data := transmute(v.z, Array<byte>)
			Memory.memcpy(pointer_cast(ref z, pointer), data.dataPtr, cast(min(data.count, sizeof(ulong)), uint))
			return z
		} else {
			abandon()
		}
	}

	unpackAsArray(v *Value) {
		if v.kind == ValueKind.ulong_ {
			return Array<byte> { dataPtr: pointer_cast(ref v.z, pointer), count: 8 }
		} else if v.kind == ValueKind.byteArray {
			return transmute(v.z, Array<byte>)^
		} else {
			abandon()
		}
	}

	copyToArray(v Value, dest *Value) {
		assert(dest.kind == ValueKind.byteArray)
		to := transmute(dest.z, Array<byte>)
		if v.kind == ValueKind.ulong_ {
			to.clearValues()
			Memory.memcpy(to.dataPtr, pointer_cast(ref v.z, pointer), cast(min(to.count, sizeof(ulong)), uint))
		} else if v.kind == ValueKind.byteArray {
			from := transmute(v.z, Array<byte>)
			if from != to {
				from.copySlice(0, from.count, to, 0)
				to.slice(from.count, to.count).clearValues()
			}
		} else {
			abandon()
		}
	}

	packArray(data Array<byte>) {
		return Value { kind: ValueKind.byteArray, z: transmute(pointer_cast(data, pointer), ulong) }
	}

	packULong(data Array<byte>) {
		z := 0_uL
		Memory.memcpy(pointer_cast(ref z, pointer), data.dataPtr, cast(min(data.count, sizeof(ulong)), uint))
		return Value { kind: ValueKind.ulong_, z: z }
	}
}
