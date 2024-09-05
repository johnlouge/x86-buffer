-- x86_buffer, John Louge
-- The default buffer system works quite great, however for the application
-- of memory segments, not quite as x86 uses little-endian byte encoding
-- and some other things relevant for how x86 handles its processor.
--
-- This version of the buffer works in little-endian.
-- Slightly modified so function comes out of the declared buffer.
--
-- Some extra foontotes might also be visible later below.
-- This program is licensed under the MIT License.

-- David Manura's purely lua bit/bit32 implementation
local M = {}
local floor = math.floor
local MOD = 2^32
local MODM = MOD-1
local function memoize(f)
  local mt = {}
  local t = setmetatable({}, mt)
  function mt:__index(k)
    local v = f(k); t[k] = v
    return v
  end
  return t
end
local function make_bitop_uncached(t, m)
  local function bitop(a, b)
    local res,p = 0,1
    while a ~= 0 and b ~= 0 do
      local am, bm = a%m, b%m
      res = res + t[am][bm]*p
      a = (a - am) / m
      b = (b - bm) / m
      p = p*m
    end
    res = res + (a+b)*p
    return res
  end
  return bitop
end
local function make_bitop(t)
  local op1 = make_bitop_uncached(t,2^1)
  local op2 = memoize(function(a)
    return memoize(function(b)
      return op1(a, b)
    end)
  end)
  return make_bitop_uncached(op2, 2^(t.n or 1))
end
function M.tobit(x) return x % 2^32 end
M.bxor = make_bitop {[0]={[0]=0,[1]=1},[1]={[0]=1,[1]=0}, n=4} local bxor = M.bxor
function M.bnot(a)   return MODM - a end local bnot = M.bnot
function M.band(a,b) return ((a+b) - bxor(a,b))/2 end local band = M.band
function M.bor(a,b)  return MODM - band(MODM - a, MODM - b) end local bor = M.bor
local lshift, rshift -- forward declare
function M.rshift(a,disp) -- Lua5.2 insipred
  if disp < 0 then return lshift(a,-disp) end
  return floor(a % 2^32 / 2^disp)
end
rshift = M.rshift
function M.lshift(a,disp) -- Lua5.2 inspired
  if disp < 0 then return rshift(a,-disp) end 
  return (a * 2^disp) % 2^32
end
lshift = M.lshift
function M.tohex(x, n) -- BitOp style
  n = n or 8
  local up
  if n <= 0 then
    if n == 0 then return '' end
    up = true
    n = - n
  end
  x = band(x, 16^n-1)
  return ('%0'..n..(up and 'X' or 'x')):format(x)
end
local tohex = M.tohex
function M.extract(n, field, width) -- Lua5.2 inspired
  width = width or 1
  return band(rshift(n, field), 2^width-1)
end
local extract = M.extract
function M.replace(n, v, field, width) -- Lua5.2 inspired
  width = width or 1
  local mask1 = 2^width-1
  v = band(v, mask1) -- required by spec?
  local mask = bnot(lshift(mask1, field))
  return band(n, mask) + lshift(v, field)
end
local replace = M.replace
function M.bswap(x)  -- BitOp style
  local a = band(x, 0xff); x = rshift(x, 8)
  local b = band(x, 0xff); x = rshift(x, 8)
  local c = band(x, 0xff); x = rshift(x, 8)
  local d = band(x, 0xff)
  return lshift(lshift(lshift(a, 8) + b, 8) + c, 8) + d
end
local bswap = M.bswap
function M.rrotate(x, disp)  -- Lua5.2 inspired
  disp = disp % 32
  local low = band(x, 2^disp-1)
  return rshift(x, disp) + lshift(low, 32-disp)
end
local rrotate = M.rrotate
function M.lrotate(x, disp)  -- Lua5.2 inspired
  return rrotate(x, -disp)
end
local lrotate = M.lrotate
M.rol = M.lrotate  -- LuaOp inspired
M.ror = M.rrotate  -- LuaOp insipred
function M.arshift(x, disp) -- Lua5.2 inspired
  local z = rshift(x, disp)
  if x >= 0x80000000 then z = z + lshift(2^disp-1, 32-disp) end
  return z
end
local arshift = M.arshift
function M.btest(x, y) -- Lua5.2 inspired
  return band(x, y) ~= 0
end


bit32 = {} -- Lua 5.2 'bit32' compatibility
local function bit32_bnot(x)
  return (-1 - x) % MOD
end
bit32.bnot = bit32_bnot

local function bit32_bxor(a, b, c, ...)
  local z
  if b then
    a = a % MOD
    b = b % MOD
    z = bxor(a, b)
    if c then
      z = bit32_bxor(z, c, ...)
    end
    return z
  elseif a then
    return a % MOD
  else
    return 0
  end
end
bit32.bxor = bit32_bxor

local function bit32_band(a, b, c, ...)
  local z
  if b then
    a = a % MOD
    b = b % MOD
    z = ((a+b) - bxor(a,b)) / 2
    if c then
      z = bit32_band(z, c, ...)
    end
    return z
  elseif a then
    return a % MOD
  else
    return MODM
  end
end
bit32.band = bit32_band

local function bit32_bor(a, b, c, ...)
  local z
  if b then
    a = a % MOD
    b = b % MOD
    z = MODM - band(MODM - a, MODM - b)
    if c then
      z = bit32_bor(z, c, ...)
    end
    return z
  elseif a then
    return a % MOD
  else
    return 0
  end
end
bit32.bor = bit32_bor
function bit32.btest(...) return bit32_band(...) ~= 0 end
function bit32.lrotate(x, disp) return lrotate(x % MOD, disp) end
function bit32.rrotate(x, disp) return rrotate(x % MOD, disp) end
function bit32.lshift(x,disp) if disp > 31 or disp < -31 then return 0 end return lshift(x % MOD, disp) end
function bit32.rshift(x,disp) if disp > 31 or disp < -31 then return 0 end return rshift(x % MOD, disp) end
function bit32.arshift(x,disp)
  x = x % MOD
  if disp >= 0 then
    if disp > 31 then
      return (x >= 0x80000000) and MODM or 0
    else
      local z = rshift(x, disp)
      if x >= 0x80000000 then z = z + lshift(2^disp-1, 32-disp) end
      return z
    end
  else
    return lshift(x, -disp)
  end
end
function bit32.extract(x, field, ...)
  local width = ... or 1
  if field < 0 or field > 31 or width < 0 or field+width > 32 then error 'out of range' end
  x = x % MOD
  return extract(x, field, ...)
end
function bit32.replace(x, v, field, ...)
  local width = ... or 1
  if field < 0 or field > 31 or width < 0 or field+width > 32 then error 'out of range' end
  x = x % MOD
  v = v % MOD
  return replace(x, v, field, ...)
end

-- Below Micah Reid's vanilla buffer implementation is used, modified for the x86_buffer
local buffer = {}
_G.bufs = {}

-- Helpers
local CHAR_SET = [[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/]]
local encode_char_set = {}
local decode_char_set = {}
for i = 1, 64 do encode_char_set[i - 1] = string.byte(CHAR_SET, i, i) decode_char_set[string.byte(CHAR_SET, i, i)] = i - 1 end
local HEX_TO_BIN = {
	["0"] = "0000", ["1"] = "0001", ["2"] = "0010", ["3"] = "0011",
	["4"] = "0100", ["5"] = "0101", ["6"] = "0110", ["7"] = "0111",
	["8"] = "1000", ["9"] = "1001", ["a"] = "1010", ["b"] = "1011",
	["c"] = "1100", ["d"] = "1101", ["e"] = "1110", ["f"] = "1111"
}
local BOOL_TO_BIT = { [true] = 1, [false] = 0 }
local CRC32_POLYNOMIAL = 0xedb88320
local crc32_poly_lookup = {}
for i = 0, 255 do
	local crc = i
	for _ = 1, 8 do
		local mask = -bit32.band(crc, 1)
		crc = bit32.bxor(bit32.rshift(crc, 1), bit32.band(CRC32_POLYNOMIAL, mask))
	end
	crc32_poly_lookup[i] = crc
end
local powers_of_2 = {} for i = 0, 64 do powers_of_2[i] = 2 ^ i end
local byte_to_hex = {} for i = 0, 255 do byte_to_hex[i] = string.format("%02x", i) end

-- Buffer creation (it is modified to also work in the context of a metatable . . .)
function buffer.create(size)
	assert(size, "buffer must have a given size")
	_G.bufs[#_G.bufs+1]={
		offset = 0, -- Added by John Louge: Custom points of positioning 
		bits = 0, -- How many free floating bits there are.
		len = 0,
		bytes = {}, --! -- Array of bytes currently in the buffer
		lastByte = 0, -- The most recent byte in the buffer, made up of free floating bits
		size = size,
		byteCount = 0, -- This variable keeps track of how many bytes there are total in the bit buffer.
		bitCount = 0, -- This variable keeps track of how many bits there are total in the bit buffer
		pointer = 0, -- This variable keeps track of what bit the read functions start at
		pointerByte = 1, -- This variable keeps track of what byte the pointer is at. It starts at 1 since the byte array starts at 1.	
	} -- Even with the given size of the allocation, bytes shall be dynamically allocated
	local buffertype = setmetatable(_G.bufs[#_G.bufs],{__index=buffer})
	return buffertype
end
-- Core dynamic allocator
function bsecure(b,pos) return (b.bytes[b.offset+pos] or 0) end
function bcaptur(b,pos,value) if b.bytes[b.offset+pos]==nil then b.len=b.len+1 end if value==0 and b.bytes[b.offset+pos]~=0 and b.bytes[b.offset+pos]~=nil then b.len=b.len-1 end b.bytes[b.offset+pos]=value end -- Not safe to check if set value is a byte size

-- Dumps
function buffer.dumpBinary(b)
	local output = {}
	for i, v in ipairs(b.bytes) do output[i] = string.gsub(byte_to_hex[v], "%x", HEX_TO_BIN) end
	if b.bits ~= 0 then output[b.byteCount] = string.sub((output[b.byteCount] or 0), 1, b.bits) end
	return table.concat(output, " ")
end
function buffer.dumpString(b)
	local output,c = {},1
	for i = 1, b.byteCount, 4096 do output[c] = string.char(table.unpack(b.bytes, i, math.min(b.byteCount, i + 4095)) or 0); c = c + 1 end
	return table.concat(output, "")
end
function buffer.dumpHex(b)
	local output = {} 
	for i,v in ipairs(b.bytes) do output[i] = byte_to_hex[v] end
	return table.concat(output, "")
end
function buffer.len(b)
	return b.len
end

-- Etc
function buffer.crc32(b)
	local crc = 0xffffffff
	for _, v in ipairs(b.bytes) do
		local poly = crc32_poly_lookup[bit32.band(bit32.bxor(crc, v), 255)]
		crc = bit32.bxor(bit32.rshift(crc, 8), poly)
	end
	return bit32.bnot(crc) % 0xffffffff
end
function buffer.getLength(b) return b.bitCount end
function buffer.getByteLength(b) return b.byteCount end
function buffer.getPointer(b) return b.pointer end
function buffer.getPointerByte(b) return b.pointerByte end
function buffer.isFinished(b) return b.pointer == b.bitCount end
function buffer.setPointer(b,n) assert(b.byteCount>=n, "not within buffer rnage") b.pointer = n b.pointerByte = math.floor(n / 8) + 1 end
function buffer.setPointerFromEnd(b,n) assert(b.byteCount>=n, "not within buffer rnage") b.pointer = b.bitCount - n b.pointerByte = math.floor(b.pointer / 8 + 1) end
function buffer.setPointerByte(b,n) assert(b.byteCount>=n, "not within buffer rnage") b.pointer = n * 8 b.pointerByte = n end
function buffer.setPointerByteFromEnd(b,n) assert(b.byteCount>=n, "not within buffer rnage") b.pointerByte = b.byteCount - n b.pointer = b.pointerByte * 8 end

-- r/ws
-- Reads
function buffer.readBits(b,n,p)
	b.offset=p or 0
	local output,byte,c = {},bsecure(b.bytes,b.pointerByte),(b.pointer % 8)
	for i = 1, n do
		local pow = powers_of_2[7 - c]
		output[i] = BOOL_TO_BIT[bit32.btest(byte, pow)] 
		c = c + 1
		if c == 8 then b.pointerByte = b.pointerByte + 1 byte = bsecure(b.bytes,b.pointerByte) c = 0 end
	end
	b.pointer = b.pointer + n 
	return output
end
function buffer.readByte(b,pos) b.offset=pos or 0 return bsecure(b,b.pointerByte) end
function buffer.readUnsigned(b,width,p)
	b.offset=p or 0
	local bytesInN, bitsInN = math.floor(width / 8), width % 8
	local n = 0
	for _ = 1, bytesInN do n = n * 0x100 n = n + buffer.readByte(b) end
	if bitsInN ~= 0 then for _, v in ipairs(buffer.readBits(b,width % 8)) do n = n * 2 n = n + v end end
	return n
end
function buffer.readString(b,p)
	b.offset=p or 0
	local stringLength = buffer.readUnsigned(b,24)
	local outputCharacters = {} for i = 1, stringLength do outputCharacters[i] = buffer.readByte(b) end
	local output,k = {},1
	for i = 1, stringLength, 4096 do output[k] = string.char(table.unpack(outputCharacters, i, math.min(stringLength, i + 4095))) k = k + 1 end
	return table.concat(output)
end
function buffer.readField(b,n,p)
	b.offset=p or 0
	local readInt = buffer.readUnsigned(b,n)
	local output = {}
	for i = n, 1, -1 do  output[i] = readInt % 2 == 1; readInt = math.floor(readInt / 2) end
	return output
end

-- Writes
function buffer.writeBits(b,p,...)
	local bitN = select("#", ...)
	b.bitCount = b.bitCount + bitN
	local packed = table.pack(...)
	b.offset=p or 0
	for _, v in ipairs(packed) do
		assert(v == 1 or v == 0, "arguments to BitBuffer.writeBits should be either 1 or 0")
		if b.bits == 0 then b.byteCount = b.byteCount + 1 end -- If the bit count is 0, increment the byteCount
		b.lastByte = b.lastByte + (v == 1 and powers_of_2[7 - b.bits] or 0) -- Add the current bit to lastByte, from right to left
		b.bits = b.bits + 1
		if b.bits == 8 then -- If the bit count is 8, set it to 0, write b.lastByte to the byte list, and set lastByte to 0
			b.bits = 0
			bcaptur(b,nil,b.lastByte)
			b.lastByte = 0
		end
	end
	if b.bits ~= 0 then bcaptur(b,nil,b.lastByte) end -- If there are some bits in lastByte, it has to be put into lastByte
end
function buffer.writeByte(b,p,n)
	n=math.clamp(n,0,255)
	if p then b.offset=p bcaptur(b,nil,n) else
		local nibble = bit32.rshift(n, b.bits) -- Shift `bits` number of bits out of `n` (they go into the aether)
		bcaptur(b,nil,b.lastByte + nibble) -- Manually set the most recent byte to the lastByte + the front part of `n`
		b.byteCount = b.byteCount + 1
		b.lastByte = bit32.band(bit32.lshift(n, 8 - b.bits), 255) -- Shift `n` forward `8-bits` and get what remains in the first 8 bits
		bcaptur(b,nilt,b.lastByte)
		b.bitCount = b.bitCount + 8 -- Increment the bit counter
	end
end
function buffer.writeUnsigned(b,width,p,n)
	local bytesInN, bitsInN = math.floor(width / 8), width % 8
	local extractedBits = {}
	if width <= 32 then
		local c = width
		for _ = 1, bytesInN do c = c - 8 buffer.writeByte(b,p,bit32.extract(n, c, 8)) end
		for i = bitsInN - 1, 0, -1 do extractedBits[bitsInN - i] = BOOL_TO_BIT[bit32.btest(n, powers_of_2[i])] end
		buffer.writeBits(b,table.unpack(extractedBits))
	else
		local leastSignificantChunk = n % 0x100000000 
		local mostSignificantChunk = math.floor(n / 0x100000000) 
		local c = width - 32
		for _ = 1, bytesInN - 4 do c = c - 8 buffer.writeByte(b,nil,bit32.extract(mostSignificantChunk, c, 8)) end
		for i = bitsInN - 1, 0, -1 do extractedBits[bitsInN - i] = BOOL_TO_BIT[bit32.btest(mostSignificantChunk, powers_of_2[i])] end
		buffer.writeBits(b,nil,table.unpack(extractedBits))
		for i = 3, 0, -1 do buffer.writeByte(b,p,bit32.extract(leastSignificantChunk, i * 8, 8)) end
	end
end
function buffer.writeField(b,p,...)
	local field,bools = 0,table.pack(...)
	for i = 1, bools.n do
		field = field * 2 -- Shift `field`. Equivalent to field<<1. At the beginning of the loop to avoid an extra shift.
		local v = bools[i]
		if v then field = field + 1 end  -- If the bit is truthy, turn it on (it defaults to off so it's fine to not have a branch)
	end
	buffer.writeUnsigned(b,bools.n,p,field)
end

function buffer.zero(b) 
	for i,v in pairs(b.bytes) do b.bytes[i]=0 end
	b.len=0 
	b.offset = 0
	b.bits = 0
	b.lastByte = 0
	b.bitCount = 0
	b.pointer = 0
	b.pointerByte = 1
end

-- The x86_buffer implementation starts below.
-- Some footnotes:
-- There is no provided seucrity on giving innapropriate values for indexing.
-- You can accidentally reverse the string readings if you are not careful!
--		* i.e: a 0>= value for the count arg in x86_buffer.writestring.
--
--
-- The little-endian byte encoding can be illustrated as the following:
--
--							  1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2
--  ADDRESS 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 . . .
--	__________________________________________________________________
--  VALUE   a b c d e f g h i j k l m n o p q r s t u v w x y z A B C . . .
--         |<--------------------->|
--			1-12 READS:        |<------------------------------->|
--			"lkjihgfedcba"		 11-27 READS:
--			IN LITTLE ENDIAN     "Azyxwvutsrqponmlk"
--								 IN LITTLE ENDIAN.
--
-- Which basically means, for read/write:
--
--							  1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2
--  ADDRESS 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 . . .
--	__________________________________________________________________
--  VALUE   a b c d e f g h i j k l m n o p q r s t u v w x y z A B C . . .
--		   |<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<|
--			READING/WRITING FROM OFFSET 21 READS/WRITES TO OFFSET 1, 
--			READING GIVES: * "utsrqponmlkjihgfedcba" *
--

local x86_buffer = {}
local float = _G.float

-- Helper functions
function toBinary(num)
	local bin = "" 
	local rem 
	while num > 0 do
		rem = num % 2 
		bin = rem .. bin 
		num = math.floor(num / 2)
	end
	return bin 
end
function realval(value)
	local newval,segs = "0.",tostring(value):split("e-")
	if segs[2] then
		newval=newval..string.rep("0",tonumber(segs[2])-1) for i,v in pairs(segs[1]:split("")) do if tonumber(v) then newval=newval..v end end return newval
	else return value end
end
function frac_tobase(n,b)
	local frac = ""
	while n~=0 do
		n=b*n
		local newn=tonumber(tostring(realval(n)):sub(1,1))
		frac=frac..newn
		n=n-newn
	end
	return frac
end
function unbin(bin)
	bin=tostring(bin)
	local append = 0
	for i=1,#bin do
		append=append+tonumber(bin:sub(#bin-i+1,#bin-i+1))*(2^(i-1))
	end
	return append
end
function frac_unbin(bin)
	bin=tostring(bin)
	local append = 0
	for i=1,#bin do
		append=append+tonumber(bin:sub(#bin-i+1,#bin-i+1))*(2^(i-1-#bin))
	end
	return append
end

-- Buffer-argument only operations. Both of which can be same.
x86_buffer.create=buffer.create
x86_buffer.len=buffer.len

-- Fill (fills repetitions count of value at offset). Reversed, of course.
-- No need to mak sure value is reversed when every byte is the same.
function x86_buffer.fill(b,offset,value,count)
	buffer.fill(b,offset-count,value,count)
end

-- Read/Write
--[[ Format for read.

	BREAKDOWN										  NAME 										CODE

	-- Unsigned values.
	|												| Byte Unsigned Integer 					"u8"
	  7											  0	  (8 bits)
	|												| Word Unsigned Integer 					"u16"
	  15										  0	  (16 bits)	
	|												| Doubleword Unsigned Integer 				"u32"
	  31										  0	  (32 bits)	
	|												| Quadword Unsigned Integer 				"u64"
	  63										  0	  (64 bits)	

	-- Signed values. Last bit is the sign always.
	|	|											| Byte Signed Integer						"s8"
Sign  7   6    									  0	  (8 bits)
	|	|											| Word Signed Integer 						"s16"
Sign  15  14									  0	  (16 bits)	
	|	|											| Doubleword Signed Integer 				"s32"
Sign  31  30									  0	  (32 bits)	
	|	|											| Quadword Signed Integer 					"s64"
Sign  63  62									  0	  (64 bits)	

	-- Floating-point precision values.
		   Integer						  Precision
	|	|			|								| Half Precision Floating Point 			"f16"
Sign  15  14	 10  9							  0	  (16 bits)	
	|	|			|								| Single Precision Floating Point 			"f32"
Sign  31  30	 23  22							  0	  (32 bits)
	|	|			|								| Double Precision Floating Point 			"f64"
Sign  63  62	 52  51							  0	  (64 bits)	
			    	  v-> *Integer bit
	|	|			|	|							| Double Extended Precision Floating Point 	"f80"
Sign  79  78	 64  63  62						  0	  (64 bits)	

*Note: This bit is specific to the double extended precision floating point type.
	   This bit decides whether a number like this is normalized or denormalized by being
	   set to 0 or 1 (denormalized or normalized). The number is denormalized when the
	   biased exponent is zero.
	   For what normalized/denormalized means: Normalized is all floating point data that can be
	   represented. Unnormalized occurs when the number is extremely small to represent and may
	   cause an underflow (TOO SMALL to represent.) You may also know "denormalized" as subnormal.
	   in extended types integer bit helps to represent such very small numbers. Nowadays it is 
	   not as significant, but according to wiki these are the reasons back in the day (8087 processor):

		   	" - Calculations can be completed a little faster if all bits 
		   	  of the significand are present in the register.
		   	  
		      - A 64 bit significand provides sufficient precision to avoid 
		      loss of precision when the results are converted back to
		      double-precision format in the vast number of cases.
		      
		      - This format provides a mechanism for indicating precision 
		      loss due to underflow which can be carried through further operations. 
		      For example, the calculation 2 × 10−4930 × 3 × 10−10 × 4 × 1020 generates 
		      the intermediate result 6 × 10−4940 which is a denormal and also involves precision loss.
		      The product of all of the terms is 24 × 10−4920 which can be represented as a normalized number.
		      The 80287 could complete this calculation and indicate the loss of precision by returning
		      an "denormal" result (exponent not 0, bit 63 = 0). Processors since the 80387 
		      no longer generate unnormals and do not support unnormal inputs to operations. 
		      They will generate a denormal if an underflow occurs but will generate a
		      normalized result if subsequent operations on the denormal can be normalized. "	   
]]
-- The float conversion function itself.
function x86_buffer.tofloat(value,precision)
	local sign = "0"
	if 0>value then value=-value sign = "1" end 

	local bias if precision==16 then bias=15 elseif precision==32 then bias=127 elseif precision==64 then bias=1023 else bias=2047 end --Else: Extended Double Precision
	local expolen if precision==16 then expolen=6 elseif precision==32 then expolen=8 elseif precision==64 then expolen=11 else expolen=15 end
	local matissalen if precision==10 then matissalen=10 elseif precision==32 then matissalen=23 elseif precision==64 then matissalen=52 else matissalen=63 end

	local value = realval(value) -- *Real* value. Since Lua tends to use the negative exponentiation at very long fractions, this conversion is done.
	local fractioned = tonumber("0"..string.sub(tostring(value),#tostring(math.floor(tonumber(value)))+1,#tostring(value))) -- Fixed fraction value
	local matissa = string.sub(toBinary(math.floor(tonumber(value)))..frac_tobase(fractioned,2),2);matissa=mantissa..string.rep("0",matissalen-#matissa) -- Matissa/Fraction. Whatever you may call it
	if matissa:sub(matissalen+1,matissalen+1)=="1" then matissa=string.sub(matissa,1,matissalen-2).."10" else matissa=string.sub(matissa,1,matissalen-2).."00" end -- Mysterious IEEE-754 roundup/down feature. To make numbers even less accurate who knows.
	local expo = string.format("%0"..expolen.."s",toBinary(bias+(((function() local amnt = 0 if tostring(value):split(".")[2] then for i,v in pairs(tostring(value):split(".")[2]:split("")) do if tonumber(v) and tonumber(v)==0 then amnt=amnt+1 end end end return amnt end)())>0 and -#frac_tobase(fractioned,2):split("1")[1]-1 or #toBinary(math.floor(tonumber(value)),2)-1)))
	return sign .. expo .. matissa
end
function x86_buffer.unfloat(value,precision)
	value = tostring(value)
	local bias if precision==16 then bias=15 elseif precision==32 then bias=127 elseif precision==64 then bias=1023 else bias=2047 end --Else: Extended Double Precision
	local expolen if precision==16 then expolen=6 elseif precision==32 then expolen=8 elseif precision==64 then expolen=11 else expolen=15 end
	local matissalen if precision==10 then matissalen=10 elseif precision==32 then matissalen=23 elseif precision==64 then matissalen=52 else matissalen=63 end

	local tolling=unbin(value:sub(2,1+expolen))-bias
	local v1,v2 = value:sub(1+expolen,1+expolen+tolling),value:sub(1+expolen+tolling+1,#value)
	local mantissa = tostring(frac_unbin(v2)):sub(3) 
  
  local resolve if table.pack(math.modf(#v1/2))[2]==0 then resolve=(2^(#v1-1)) else resolve=0 end
	return (value:sub(1,1)=="1" and "-" or "") .. (math.floor(tonumber(unbin(v1)+resolve))) .. (mantissa=="" and mantissa or "." .. mantissa) 
end

-- For simplification, instead of all the seperate r/w
-- functions, here are two.
-- First argument can be of the predescribed forms above,
-- second if offset, for write third is a numeric value.
function x86_buffer.read(b,mode,offset)
	local t,a = string.sub(mode,1,1),tonumber(string.sub(mode,2,#mode))/8
	if t and a then
		local value = 0
		for i=0,a do value = bit32.bor(value,buffer.readByte(b,offset-a+i)*(2^(i*8))) end
		if t=="u" then
			return value
		elseif t=="s" then 
			return value>=0x80*(0x100^a/0x100) and -value or value
		elseif t=="f" then
			return x86_buffer.unfloat(value)
		end
	end
end

-- For writing, the mode is not considered.
-- Since different instructions and operations
-- can interpret a value differently (one may
-- use the value in signed comprehension, another
-- in float) the writing is always signed, during
-- the use of the read function above the mode can
-- be decided. Offset is a number, value is a number.
function x86_buffer.write(b,offset,value)
	local offsetting = math.ceil(math.log(value,2))/8
	for i=0,offsetting do
		buffer.writeByte(b,offset-offsetting+i,bit32.rrotate(value,0x8*(i)))
	end
end

-- Functions building on r/w!
-- String manipulation.
x86_buffer.tostring = buffer.dumpString
x86_buffer.fromstring = function(str,mode,offset)
	offset = (offset or 1)-#str
	str = string.reverse(str) local strt = str:split("")
	local buff = buffer.create(#str)
	for i=offset,offset+#str-1 do x86_buffer.write(buff,offset,strt[i-offset+1]) end
	return buff
end
x86_buffer.readstring=function(b,offset,count)
	local str = ""
	for i=offset,offset-count+1 do
		str=str..string.char(x86_buffer.read(b,"u8",i))
	end
	return str
end
x86_buffer.revrstring=function(b,offset,count)
	return string.reverse(x86_buffer.readstring(b,offset,count))
end
x86_buffer.writestring=function(b,offset,value,count)
	local strt = value:split("")
	if count==0 then return "" end
	for i=offset,offset-(count or #strt)+1 do
		x86_buffer.write(b,i,strt[offset-i+1])
	end
end
x86_buffer.clear=function(b) buffer.zero(b) end

return x86_buffer
