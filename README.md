# x86-buffer
The default buffer system works quite great, however for the application of memory segments, not quite as x86 uses little-endian byte encoding and some other things relevant for how x86 handles its processor.
So this is a simple and conscise Lua script which helps with can assist in the recreation of the behaviours of memory operations in the x86 architecture.
Below will be visible all of the functions the module provides.

This program is licensed under the MIT License (same as Lua license).

This program uses the lua-bit-numberlia module of David Manura, and the vanilla buffer of Micah Reid.

### **Legend:**

| Method        | Behaviour     | Input  | Output |
| :-------------: |-----|:----| ----------:|
| | | |
| Common | Generic functionality. | | |
| x86_buffer.create      | Creates a buffer | size:number (decimal max buffer length) | buffer |
| x86_buffer.len      | Length of buffer | created buffer | number (LENGTH of the buffer) |
| x86_buffer.size      | Size of buffer | created buffer | number (UPTAKEN SIZE of the buffer) |
| x86_buffer.fill      | Fills given buffer from given offset with the given value count times | buffer, offset:number, value:number, count:number | nil |
| x86_buffer.clear      | Clears entire buffer (blanks every allocated offset) | buffer, offset:number, value:number, count:number | nil |
| | | |
| r/w operations | Read/write main functionality. | | |
| x86_buffer.read      | Reads buffer from certain offset in specified mode. Modes can be a string ("s"/"u"/"f")+("16"/"32"/"64"/"80") i.e: "s32","f64"... | buffer,mode,offset:number | value:numbered (the value read, depending on specified mode) |
| x86_buffer.write      | Writes a value to buffer from certain offset. Value can be any, writing automatically writes to that length. | buffer,offset:number,value:number | nil |
| | | |
| read conversions | Conversion functions for values returned from read. | | |
| x86_buffer.tofloat      | Helps convert floating point number to binary according to IEEE-754 standard (lua-IEEE754) | value:number, precision:number (bits to determine precision: 16/32/64/80.) | string (representing the binary value) |
| x86_buffer.unfloat      | Unconverts value returned by x86_buffer.tofloat; Take notice IEEE-754 is lossy binary conversion. Returned value won't be the precise value passed to x86_buffer.tofloat | value:string (binary value returned by x86_buffer.tofloat),precision:number | value:number (unconverged floating point number) |
| | | |
| string operations | Buffer operations relative to strings. | | |
| x86_buffer.tostring      | Converts and returns passed buffer as string. | buffer | buffer_string:string |
| x86_buffer.fromstring      | Converts and returns passed string as buffer. | string, offset:number (custom offset is optional) | buffer |
| x86_buffer.readstring      | Reads a certain range of the buffer from a given offset and converts to a string. | buffer, offset:number, count (amount of bytes to read) | string |
| x86_buffer.revrstring      | Same operation as x86_buffer.readstring but in reversed form. (Provides string that would be read by big-endian byte encoding) | buffer, offset:number, count (amount of bytes to read) | string |
| x86_buffer.writestring      | Writes a given string at a certain offset as its own byte representation into the buffer, up to an optional count. If count is 0, nothing is written. If count is nil, the whole string is written. | buffer,offset:number,value:string,count:number (optional max amount of bytes to write), | nil |
| | | |

###### 2024 (c) John Louge
