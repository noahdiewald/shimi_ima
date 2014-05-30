// # Globals object
//
// A place to temporarily store global objects. Sometimes this is more
// convenient than using other types of client side storage. It is used
// rarely and explicitly using this object.
//
// This is not loaded as a module like other code here. It is concatenated
// to the beginning of the target JavaScript file created by the build
// process.

var globals = {};

require=(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);throw new Error("Cannot find module '"+o+"'")}var f=n[o]={exports:{}};t[o][0].call(f.exports,function(e){var n=t[o][1][e];return s(n?n:e)},f,f.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
/*!
 * The buffer module from node.js, for the browser.
 *
 * @author   Feross Aboukhadijeh <feross@feross.org> <http://feross.org>
 * @license  MIT
 */

var base64 = require('base64-js')
var ieee754 = require('ieee754')

exports.Buffer = Buffer
exports.SlowBuffer = Buffer
exports.INSPECT_MAX_BYTES = 50
Buffer.poolSize = 8192

/**
 * If `Buffer._useTypedArrays`:
 *   === true    Use Uint8Array implementation (fastest)
 *   === false   Use Object implementation (compatible down to IE6)
 */
Buffer._useTypedArrays = (function () {
  // Detect if browser supports Typed Arrays. Supported browsers are IE 10+, Firefox 4+,
  // Chrome 7+, Safari 5.1+, Opera 11.6+, iOS 4.2+. If the browser does not support adding
  // properties to `Uint8Array` instances, then that's the same as no `Uint8Array` support
  // because we need to be able to add all the node Buffer API methods. This is an issue
  // in Firefox 4-29. Now fixed: https://bugzilla.mozilla.org/show_bug.cgi?id=695438
  try {
    var buf = new ArrayBuffer(0)
    var arr = new Uint8Array(buf)
    arr.foo = function () { return 42 }
    return 42 === arr.foo() &&
        typeof arr.subarray === 'function' // Chrome 9-10 lack `subarray`
  } catch (e) {
    return false
  }
})()

/**
 * Class: Buffer
 * =============
 *
 * The Buffer constructor returns instances of `Uint8Array` that are augmented
 * with function properties for all the node `Buffer` API functions. We use
 * `Uint8Array` so that square bracket notation works as expected -- it returns
 * a single octet.
 *
 * By augmenting the instances, we can avoid modifying the `Uint8Array`
 * prototype.
 */
function Buffer (subject, encoding, noZero) {
  if (!(this instanceof Buffer))
    return new Buffer(subject, encoding, noZero)

  var type = typeof subject

  // Workaround: node's base64 implementation allows for non-padded strings
  // while base64-js does not.
  if (encoding === 'base64' && type === 'string') {
    subject = stringtrim(subject)
    while (subject.length % 4 !== 0) {
      subject = subject + '='
    }
  }

  // Find the length
  var length
  if (type === 'number')
    length = coerce(subject)
  else if (type === 'string')
    length = Buffer.byteLength(subject, encoding)
  else if (type === 'object')
    length = coerce(subject.length) // assume that object is array-like
  else
    throw new Error('First argument needs to be a number, array or string.')

  var buf
  if (Buffer._useTypedArrays) {
    // Preferred: Return an augmented `Uint8Array` instance for best performance
    buf = Buffer._augment(new Uint8Array(length))
  } else {
    // Fallback: Return THIS instance of Buffer (created by `new`)
    buf = this
    buf.length = length
    buf._isBuffer = true
  }

  var i
  if (Buffer._useTypedArrays && typeof subject.byteLength === 'number') {
    // Speed optimization -- use set if we're copying from a typed array
    buf._set(subject)
  } else if (isArrayish(subject)) {
    // Treat array-ish objects as a byte array
    for (i = 0; i < length; i++) {
      if (Buffer.isBuffer(subject))
        buf[i] = subject.readUInt8(i)
      else
        buf[i] = subject[i]
    }
  } else if (type === 'string') {
    buf.write(subject, 0, encoding)
  } else if (type === 'number' && !Buffer._useTypedArrays && !noZero) {
    for (i = 0; i < length; i++) {
      buf[i] = 0
    }
  }

  return buf
}

// STATIC METHODS
// ==============

Buffer.isEncoding = function (encoding) {
  switch (String(encoding).toLowerCase()) {
    case 'hex':
    case 'utf8':
    case 'utf-8':
    case 'ascii':
    case 'binary':
    case 'base64':
    case 'raw':
    case 'ucs2':
    case 'ucs-2':
    case 'utf16le':
    case 'utf-16le':
      return true
    default:
      return false
  }
}

Buffer.isBuffer = function (b) {
  return !!(b !== null && b !== undefined && b._isBuffer)
}

Buffer.byteLength = function (str, encoding) {
  var ret
  str = str + ''
  switch (encoding || 'utf8') {
    case 'hex':
      ret = str.length / 2
      break
    case 'utf8':
    case 'utf-8':
      ret = utf8ToBytes(str).length
      break
    case 'ascii':
    case 'binary':
    case 'raw':
      ret = str.length
      break
    case 'base64':
      ret = base64ToBytes(str).length
      break
    case 'ucs2':
    case 'ucs-2':
    case 'utf16le':
    case 'utf-16le':
      ret = str.length * 2
      break
    default:
      throw new Error('Unknown encoding')
  }
  return ret
}

Buffer.concat = function (list, totalLength) {
  assert(isArray(list), 'Usage: Buffer.concat(list, [totalLength])\n' +
      'list should be an Array.')

  if (list.length === 0) {
    return new Buffer(0)
  } else if (list.length === 1) {
    return list[0]
  }

  var i
  if (typeof totalLength !== 'number') {
    totalLength = 0
    for (i = 0; i < list.length; i++) {
      totalLength += list[i].length
    }
  }

  var buf = new Buffer(totalLength)
  var pos = 0
  for (i = 0; i < list.length; i++) {
    var item = list[i]
    item.copy(buf, pos)
    pos += item.length
  }
  return buf
}

// BUFFER INSTANCE METHODS
// =======================

function _hexWrite (buf, string, offset, length) {
  offset = Number(offset) || 0
  var remaining = buf.length - offset
  if (!length) {
    length = remaining
  } else {
    length = Number(length)
    if (length > remaining) {
      length = remaining
    }
  }

  // must be an even number of digits
  var strLen = string.length
  assert(strLen % 2 === 0, 'Invalid hex string')

  if (length > strLen / 2) {
    length = strLen / 2
  }
  for (var i = 0; i < length; i++) {
    var byte = parseInt(string.substr(i * 2, 2), 16)
    assert(!isNaN(byte), 'Invalid hex string')
    buf[offset + i] = byte
  }
  Buffer._charsWritten = i * 2
  return i
}

function _utf8Write (buf, string, offset, length) {
  var charsWritten = Buffer._charsWritten =
    blitBuffer(utf8ToBytes(string), buf, offset, length)
  return charsWritten
}

function _asciiWrite (buf, string, offset, length) {
  var charsWritten = Buffer._charsWritten =
    blitBuffer(asciiToBytes(string), buf, offset, length)
  return charsWritten
}

function _binaryWrite (buf, string, offset, length) {
  return _asciiWrite(buf, string, offset, length)
}

function _base64Write (buf, string, offset, length) {
  var charsWritten = Buffer._charsWritten =
    blitBuffer(base64ToBytes(string), buf, offset, length)
  return charsWritten
}

function _utf16leWrite (buf, string, offset, length) {
  var charsWritten = Buffer._charsWritten =
    blitBuffer(utf16leToBytes(string), buf, offset, length)
  return charsWritten
}

Buffer.prototype.write = function (string, offset, length, encoding) {
  // Support both (string, offset, length, encoding)
  // and the legacy (string, encoding, offset, length)
  if (isFinite(offset)) {
    if (!isFinite(length)) {
      encoding = length
      length = undefined
    }
  } else {  // legacy
    var swap = encoding
    encoding = offset
    offset = length
    length = swap
  }

  offset = Number(offset) || 0
  var remaining = this.length - offset
  if (!length) {
    length = remaining
  } else {
    length = Number(length)
    if (length > remaining) {
      length = remaining
    }
  }
  encoding = String(encoding || 'utf8').toLowerCase()

  var ret
  switch (encoding) {
    case 'hex':
      ret = _hexWrite(this, string, offset, length)
      break
    case 'utf8':
    case 'utf-8':
      ret = _utf8Write(this, string, offset, length)
      break
    case 'ascii':
      ret = _asciiWrite(this, string, offset, length)
      break
    case 'binary':
      ret = _binaryWrite(this, string, offset, length)
      break
    case 'base64':
      ret = _base64Write(this, string, offset, length)
      break
    case 'ucs2':
    case 'ucs-2':
    case 'utf16le':
    case 'utf-16le':
      ret = _utf16leWrite(this, string, offset, length)
      break
    default:
      throw new Error('Unknown encoding')
  }
  return ret
}

Buffer.prototype.toString = function (encoding, start, end) {
  var self = this

  encoding = String(encoding || 'utf8').toLowerCase()
  start = Number(start) || 0
  end = (end !== undefined)
    ? Number(end)
    : end = self.length

  // Fastpath empty strings
  if (end === start)
    return ''

  var ret
  switch (encoding) {
    case 'hex':
      ret = _hexSlice(self, start, end)
      break
    case 'utf8':
    case 'utf-8':
      ret = _utf8Slice(self, start, end)
      break
    case 'ascii':
      ret = _asciiSlice(self, start, end)
      break
    case 'binary':
      ret = _binarySlice(self, start, end)
      break
    case 'base64':
      ret = _base64Slice(self, start, end)
      break
    case 'ucs2':
    case 'ucs-2':
    case 'utf16le':
    case 'utf-16le':
      ret = _utf16leSlice(self, start, end)
      break
    default:
      throw new Error('Unknown encoding')
  }
  return ret
}

Buffer.prototype.toJSON = function () {
  return {
    type: 'Buffer',
    data: Array.prototype.slice.call(this._arr || this, 0)
  }
}

// copy(targetBuffer, targetStart=0, sourceStart=0, sourceEnd=buffer.length)
Buffer.prototype.copy = function (target, target_start, start, end) {
  var source = this

  if (!start) start = 0
  if (!end && end !== 0) end = this.length
  if (!target_start) target_start = 0

  // Copy 0 bytes; we're done
  if (end === start) return
  if (target.length === 0 || source.length === 0) return

  // Fatal error conditions
  assert(end >= start, 'sourceEnd < sourceStart')
  assert(target_start >= 0 && target_start < target.length,
      'targetStart out of bounds')
  assert(start >= 0 && start < source.length, 'sourceStart out of bounds')
  assert(end >= 0 && end <= source.length, 'sourceEnd out of bounds')

  // Are we oob?
  if (end > this.length)
    end = this.length
  if (target.length - target_start < end - start)
    end = target.length - target_start + start

  var len = end - start

  if (len < 100 || !Buffer._useTypedArrays) {
    for (var i = 0; i < len; i++)
      target[i + target_start] = this[i + start]
  } else {
    target._set(this.subarray(start, start + len), target_start)
  }
}

function _base64Slice (buf, start, end) {
  if (start === 0 && end === buf.length) {
    return base64.fromByteArray(buf)
  } else {
    return base64.fromByteArray(buf.slice(start, end))
  }
}

function _utf8Slice (buf, start, end) {
  var res = ''
  var tmp = ''
  end = Math.min(buf.length, end)

  for (var i = start; i < end; i++) {
    if (buf[i] <= 0x7F) {
      res += decodeUtf8Char(tmp) + String.fromCharCode(buf[i])
      tmp = ''
    } else {
      tmp += '%' + buf[i].toString(16)
    }
  }

  return res + decodeUtf8Char(tmp)
}

function _asciiSlice (buf, start, end) {
  var ret = ''
  end = Math.min(buf.length, end)

  for (var i = start; i < end; i++)
    ret += String.fromCharCode(buf[i])
  return ret
}

function _binarySlice (buf, start, end) {
  return _asciiSlice(buf, start, end)
}

function _hexSlice (buf, start, end) {
  var len = buf.length

  if (!start || start < 0) start = 0
  if (!end || end < 0 || end > len) end = len

  var out = ''
  for (var i = start; i < end; i++) {
    out += toHex(buf[i])
  }
  return out
}

function _utf16leSlice (buf, start, end) {
  var bytes = buf.slice(start, end)
  var res = ''
  for (var i = 0; i < bytes.length; i += 2) {
    res += String.fromCharCode(bytes[i] + bytes[i+1] * 256)
  }
  return res
}

Buffer.prototype.slice = function (start, end) {
  var len = this.length
  start = clamp(start, len, 0)
  end = clamp(end, len, len)

  if (Buffer._useTypedArrays) {
    return Buffer._augment(this.subarray(start, end))
  } else {
    var sliceLen = end - start
    var newBuf = new Buffer(sliceLen, undefined, true)
    for (var i = 0; i < sliceLen; i++) {
      newBuf[i] = this[i + start]
    }
    return newBuf
  }
}

// `get` will be removed in Node 0.13+
Buffer.prototype.get = function (offset) {
  console.log('.get() is deprecated. Access using array indexes instead.')
  return this.readUInt8(offset)
}

// `set` will be removed in Node 0.13+
Buffer.prototype.set = function (v, offset) {
  console.log('.set() is deprecated. Access using array indexes instead.')
  return this.writeUInt8(v, offset)
}

Buffer.prototype.readUInt8 = function (offset, noAssert) {
  if (!noAssert) {
    assert(offset !== undefined && offset !== null, 'missing offset')
    assert(offset < this.length, 'Trying to read beyond buffer length')
  }

  if (offset >= this.length)
    return

  return this[offset]
}

function _readUInt16 (buf, offset, littleEndian, noAssert) {
  if (!noAssert) {
    assert(typeof littleEndian === 'boolean', 'missing or invalid endian')
    assert(offset !== undefined && offset !== null, 'missing offset')
    assert(offset + 1 < buf.length, 'Trying to read beyond buffer length')
  }

  var len = buf.length
  if (offset >= len)
    return

  var val
  if (littleEndian) {
    val = buf[offset]
    if (offset + 1 < len)
      val |= buf[offset + 1] << 8
  } else {
    val = buf[offset] << 8
    if (offset + 1 < len)
      val |= buf[offset + 1]
  }
  return val
}

Buffer.prototype.readUInt16LE = function (offset, noAssert) {
  return _readUInt16(this, offset, true, noAssert)
}

Buffer.prototype.readUInt16BE = function (offset, noAssert) {
  return _readUInt16(this, offset, false, noAssert)
}

function _readUInt32 (buf, offset, littleEndian, noAssert) {
  if (!noAssert) {
    assert(typeof littleEndian === 'boolean', 'missing or invalid endian')
    assert(offset !== undefined && offset !== null, 'missing offset')
    assert(offset + 3 < buf.length, 'Trying to read beyond buffer length')
  }

  var len = buf.length
  if (offset >= len)
    return

  var val
  if (littleEndian) {
    if (offset + 2 < len)
      val = buf[offset + 2] << 16
    if (offset + 1 < len)
      val |= buf[offset + 1] << 8
    val |= buf[offset]
    if (offset + 3 < len)
      val = val + (buf[offset + 3] << 24 >>> 0)
  } else {
    if (offset + 1 < len)
      val = buf[offset + 1] << 16
    if (offset + 2 < len)
      val |= buf[offset + 2] << 8
    if (offset + 3 < len)
      val |= buf[offset + 3]
    val = val + (buf[offset] << 24 >>> 0)
  }
  return val
}

Buffer.prototype.readUInt32LE = function (offset, noAssert) {
  return _readUInt32(this, offset, true, noAssert)
}

Buffer.prototype.readUInt32BE = function (offset, noAssert) {
  return _readUInt32(this, offset, false, noAssert)
}

Buffer.prototype.readInt8 = function (offset, noAssert) {
  if (!noAssert) {
    assert(offset !== undefined && offset !== null,
        'missing offset')
    assert(offset < this.length, 'Trying to read beyond buffer length')
  }

  if (offset >= this.length)
    return

  var neg = this[offset] & 0x80
  if (neg)
    return (0xff - this[offset] + 1) * -1
  else
    return this[offset]
}

function _readInt16 (buf, offset, littleEndian, noAssert) {
  if (!noAssert) {
    assert(typeof littleEndian === 'boolean', 'missing or invalid endian')
    assert(offset !== undefined && offset !== null, 'missing offset')
    assert(offset + 1 < buf.length, 'Trying to read beyond buffer length')
  }

  var len = buf.length
  if (offset >= len)
    return

  var val = _readUInt16(buf, offset, littleEndian, true)
  var neg = val & 0x8000
  if (neg)
    return (0xffff - val + 1) * -1
  else
    return val
}

Buffer.prototype.readInt16LE = function (offset, noAssert) {
  return _readInt16(this, offset, true, noAssert)
}

Buffer.prototype.readInt16BE = function (offset, noAssert) {
  return _readInt16(this, offset, false, noAssert)
}

function _readInt32 (buf, offset, littleEndian, noAssert) {
  if (!noAssert) {
    assert(typeof littleEndian === 'boolean', 'missing or invalid endian')
    assert(offset !== undefined && offset !== null, 'missing offset')
    assert(offset + 3 < buf.length, 'Trying to read beyond buffer length')
  }

  var len = buf.length
  if (offset >= len)
    return

  var val = _readUInt32(buf, offset, littleEndian, true)
  var neg = val & 0x80000000
  if (neg)
    return (0xffffffff - val + 1) * -1
  else
    return val
}

Buffer.prototype.readInt32LE = function (offset, noAssert) {
  return _readInt32(this, offset, true, noAssert)
}

Buffer.prototype.readInt32BE = function (offset, noAssert) {
  return _readInt32(this, offset, false, noAssert)
}

function _readFloat (buf, offset, littleEndian, noAssert) {
  if (!noAssert) {
    assert(typeof littleEndian === 'boolean', 'missing or invalid endian')
    assert(offset + 3 < buf.length, 'Trying to read beyond buffer length')
  }

  return ieee754.read(buf, offset, littleEndian, 23, 4)
}

Buffer.prototype.readFloatLE = function (offset, noAssert) {
  return _readFloat(this, offset, true, noAssert)
}

Buffer.prototype.readFloatBE = function (offset, noAssert) {
  return _readFloat(this, offset, false, noAssert)
}

function _readDouble (buf, offset, littleEndian, noAssert) {
  if (!noAssert) {
    assert(typeof littleEndian === 'boolean', 'missing or invalid endian')
    assert(offset + 7 < buf.length, 'Trying to read beyond buffer length')
  }

  return ieee754.read(buf, offset, littleEndian, 52, 8)
}

Buffer.prototype.readDoubleLE = function (offset, noAssert) {
  return _readDouble(this, offset, true, noAssert)
}

Buffer.prototype.readDoubleBE = function (offset, noAssert) {
  return _readDouble(this, offset, false, noAssert)
}

Buffer.prototype.writeUInt8 = function (value, offset, noAssert) {
  if (!noAssert) {
    assert(value !== undefined && value !== null, 'missing value')
    assert(offset !== undefined && offset !== null, 'missing offset')
    assert(offset < this.length, 'trying to write beyond buffer length')
    verifuint(value, 0xff)
  }

  if (offset >= this.length) return

  this[offset] = value
}

function _writeUInt16 (buf, value, offset, littleEndian, noAssert) {
  if (!noAssert) {
    assert(value !== undefined && value !== null, 'missing value')
    assert(typeof littleEndian === 'boolean', 'missing or invalid endian')
    assert(offset !== undefined && offset !== null, 'missing offset')
    assert(offset + 1 < buf.length, 'trying to write beyond buffer length')
    verifuint(value, 0xffff)
  }

  var len = buf.length
  if (offset >= len)
    return

  for (var i = 0, j = Math.min(len - offset, 2); i < j; i++) {
    buf[offset + i] =
        (value & (0xff << (8 * (littleEndian ? i : 1 - i)))) >>>
            (littleEndian ? i : 1 - i) * 8
  }
}

Buffer.prototype.writeUInt16LE = function (value, offset, noAssert) {
  _writeUInt16(this, value, offset, true, noAssert)
}

Buffer.prototype.writeUInt16BE = function (value, offset, noAssert) {
  _writeUInt16(this, value, offset, false, noAssert)
}

function _writeUInt32 (buf, value, offset, littleEndian, noAssert) {
  if (!noAssert) {
    assert(value !== undefined && value !== null, 'missing value')
    assert(typeof littleEndian === 'boolean', 'missing or invalid endian')
    assert(offset !== undefined && offset !== null, 'missing offset')
    assert(offset + 3 < buf.length, 'trying to write beyond buffer length')
    verifuint(value, 0xffffffff)
  }

  var len = buf.length
  if (offset >= len)
    return

  for (var i = 0, j = Math.min(len - offset, 4); i < j; i++) {
    buf[offset + i] =
        (value >>> (littleEndian ? i : 3 - i) * 8) & 0xff
  }
}

Buffer.prototype.writeUInt32LE = function (value, offset, noAssert) {
  _writeUInt32(this, value, offset, true, noAssert)
}

Buffer.prototype.writeUInt32BE = function (value, offset, noAssert) {
  _writeUInt32(this, value, offset, false, noAssert)
}

Buffer.prototype.writeInt8 = function (value, offset, noAssert) {
  if (!noAssert) {
    assert(value !== undefined && value !== null, 'missing value')
    assert(offset !== undefined && offset !== null, 'missing offset')
    assert(offset < this.length, 'Trying to write beyond buffer length')
    verifsint(value, 0x7f, -0x80)
  }

  if (offset >= this.length)
    return

  if (value >= 0)
    this.writeUInt8(value, offset, noAssert)
  else
    this.writeUInt8(0xff + value + 1, offset, noAssert)
}

function _writeInt16 (buf, value, offset, littleEndian, noAssert) {
  if (!noAssert) {
    assert(value !== undefined && value !== null, 'missing value')
    assert(typeof littleEndian === 'boolean', 'missing or invalid endian')
    assert(offset !== undefined && offset !== null, 'missing offset')
    assert(offset + 1 < buf.length, 'Trying to write beyond buffer length')
    verifsint(value, 0x7fff, -0x8000)
  }

  var len = buf.length
  if (offset >= len)
    return

  if (value >= 0)
    _writeUInt16(buf, value, offset, littleEndian, noAssert)
  else
    _writeUInt16(buf, 0xffff + value + 1, offset, littleEndian, noAssert)
}

Buffer.prototype.writeInt16LE = function (value, offset, noAssert) {
  _writeInt16(this, value, offset, true, noAssert)
}

Buffer.prototype.writeInt16BE = function (value, offset, noAssert) {
  _writeInt16(this, value, offset, false, noAssert)
}

function _writeInt32 (buf, value, offset, littleEndian, noAssert) {
  if (!noAssert) {
    assert(value !== undefined && value !== null, 'missing value')
    assert(typeof littleEndian === 'boolean', 'missing or invalid endian')
    assert(offset !== undefined && offset !== null, 'missing offset')
    assert(offset + 3 < buf.length, 'Trying to write beyond buffer length')
    verifsint(value, 0x7fffffff, -0x80000000)
  }

  var len = buf.length
  if (offset >= len)
    return

  if (value >= 0)
    _writeUInt32(buf, value, offset, littleEndian, noAssert)
  else
    _writeUInt32(buf, 0xffffffff + value + 1, offset, littleEndian, noAssert)
}

Buffer.prototype.writeInt32LE = function (value, offset, noAssert) {
  _writeInt32(this, value, offset, true, noAssert)
}

Buffer.prototype.writeInt32BE = function (value, offset, noAssert) {
  _writeInt32(this, value, offset, false, noAssert)
}

function _writeFloat (buf, value, offset, littleEndian, noAssert) {
  if (!noAssert) {
    assert(value !== undefined && value !== null, 'missing value')
    assert(typeof littleEndian === 'boolean', 'missing or invalid endian')
    assert(offset !== undefined && offset !== null, 'missing offset')
    assert(offset + 3 < buf.length, 'Trying to write beyond buffer length')
    verifIEEE754(value, 3.4028234663852886e+38, -3.4028234663852886e+38)
  }

  var len = buf.length
  if (offset >= len)
    return

  ieee754.write(buf, value, offset, littleEndian, 23, 4)
}

Buffer.prototype.writeFloatLE = function (value, offset, noAssert) {
  _writeFloat(this, value, offset, true, noAssert)
}

Buffer.prototype.writeFloatBE = function (value, offset, noAssert) {
  _writeFloat(this, value, offset, false, noAssert)
}

function _writeDouble (buf, value, offset, littleEndian, noAssert) {
  if (!noAssert) {
    assert(value !== undefined && value !== null, 'missing value')
    assert(typeof littleEndian === 'boolean', 'missing or invalid endian')
    assert(offset !== undefined && offset !== null, 'missing offset')
    assert(offset + 7 < buf.length,
        'Trying to write beyond buffer length')
    verifIEEE754(value, 1.7976931348623157E+308, -1.7976931348623157E+308)
  }

  var len = buf.length
  if (offset >= len)
    return

  ieee754.write(buf, value, offset, littleEndian, 52, 8)
}

Buffer.prototype.writeDoubleLE = function (value, offset, noAssert) {
  _writeDouble(this, value, offset, true, noAssert)
}

Buffer.prototype.writeDoubleBE = function (value, offset, noAssert) {
  _writeDouble(this, value, offset, false, noAssert)
}

// fill(value, start=0, end=buffer.length)
Buffer.prototype.fill = function (value, start, end) {
  if (!value) value = 0
  if (!start) start = 0
  if (!end) end = this.length

  if (typeof value === 'string') {
    value = value.charCodeAt(0)
  }

  assert(typeof value === 'number' && !isNaN(value), 'value is not a number')
  assert(end >= start, 'end < start')

  // Fill 0 bytes; we're done
  if (end === start) return
  if (this.length === 0) return

  assert(start >= 0 && start < this.length, 'start out of bounds')
  assert(end >= 0 && end <= this.length, 'end out of bounds')

  for (var i = start; i < end; i++) {
    this[i] = value
  }
}

Buffer.prototype.inspect = function () {
  var out = []
  var len = this.length
  for (var i = 0; i < len; i++) {
    out[i] = toHex(this[i])
    if (i === exports.INSPECT_MAX_BYTES) {
      out[i + 1] = '...'
      break
    }
  }
  return '<Buffer ' + out.join(' ') + '>'
}

/**
 * Creates a new `ArrayBuffer` with the *copied* memory of the buffer instance.
 * Added in Node 0.12. Only available in browsers that support ArrayBuffer.
 */
Buffer.prototype.toArrayBuffer = function () {
  if (typeof Uint8Array !== 'undefined') {
    if (Buffer._useTypedArrays) {
      return (new Buffer(this)).buffer
    } else {
      var buf = new Uint8Array(this.length)
      for (var i = 0, len = buf.length; i < len; i += 1)
        buf[i] = this[i]
      return buf.buffer
    }
  } else {
    throw new Error('Buffer.toArrayBuffer not supported in this browser')
  }
}

// HELPER FUNCTIONS
// ================

function stringtrim (str) {
  if (str.trim) return str.trim()
  return str.replace(/^\s+|\s+$/g, '')
}

var BP = Buffer.prototype

/**
 * Augment a Uint8Array *instance* (not the Uint8Array class!) with Buffer methods
 */
Buffer._augment = function (arr) {
  arr._isBuffer = true

  // save reference to original Uint8Array get/set methods before overwriting
  arr._get = arr.get
  arr._set = arr.set

  // deprecated, will be removed in node 0.13+
  arr.get = BP.get
  arr.set = BP.set

  arr.write = BP.write
  arr.toString = BP.toString
  arr.toLocaleString = BP.toString
  arr.toJSON = BP.toJSON
  arr.copy = BP.copy
  arr.slice = BP.slice
  arr.readUInt8 = BP.readUInt8
  arr.readUInt16LE = BP.readUInt16LE
  arr.readUInt16BE = BP.readUInt16BE
  arr.readUInt32LE = BP.readUInt32LE
  arr.readUInt32BE = BP.readUInt32BE
  arr.readInt8 = BP.readInt8
  arr.readInt16LE = BP.readInt16LE
  arr.readInt16BE = BP.readInt16BE
  arr.readInt32LE = BP.readInt32LE
  arr.readInt32BE = BP.readInt32BE
  arr.readFloatLE = BP.readFloatLE
  arr.readFloatBE = BP.readFloatBE
  arr.readDoubleLE = BP.readDoubleLE
  arr.readDoubleBE = BP.readDoubleBE
  arr.writeUInt8 = BP.writeUInt8
  arr.writeUInt16LE = BP.writeUInt16LE
  arr.writeUInt16BE = BP.writeUInt16BE
  arr.writeUInt32LE = BP.writeUInt32LE
  arr.writeUInt32BE = BP.writeUInt32BE
  arr.writeInt8 = BP.writeInt8
  arr.writeInt16LE = BP.writeInt16LE
  arr.writeInt16BE = BP.writeInt16BE
  arr.writeInt32LE = BP.writeInt32LE
  arr.writeInt32BE = BP.writeInt32BE
  arr.writeFloatLE = BP.writeFloatLE
  arr.writeFloatBE = BP.writeFloatBE
  arr.writeDoubleLE = BP.writeDoubleLE
  arr.writeDoubleBE = BP.writeDoubleBE
  arr.fill = BP.fill
  arr.inspect = BP.inspect
  arr.toArrayBuffer = BP.toArrayBuffer

  return arr
}

// slice(start, end)
function clamp (index, len, defaultValue) {
  if (typeof index !== 'number') return defaultValue
  index = ~~index;  // Coerce to integer.
  if (index >= len) return len
  if (index >= 0) return index
  index += len
  if (index >= 0) return index
  return 0
}

function coerce (length) {
  // Coerce length to a number (possibly NaN), round up
  // in case it's fractional (e.g. 123.456) then do a
  // double negate to coerce a NaN to 0. Easy, right?
  length = ~~Math.ceil(+length)
  return length < 0 ? 0 : length
}

function isArray (subject) {
  return (Array.isArray || function (subject) {
    return Object.prototype.toString.call(subject) === '[object Array]'
  })(subject)
}

function isArrayish (subject) {
  return isArray(subject) || Buffer.isBuffer(subject) ||
      subject && typeof subject === 'object' &&
      typeof subject.length === 'number'
}

function toHex (n) {
  if (n < 16) return '0' + n.toString(16)
  return n.toString(16)
}

function utf8ToBytes (str) {
  var byteArray = []
  for (var i = 0; i < str.length; i++) {
    var b = str.charCodeAt(i)
    if (b <= 0x7F)
      byteArray.push(str.charCodeAt(i))
    else {
      var start = i
      if (b >= 0xD800 && b <= 0xDFFF) i++
      var h = encodeURIComponent(str.slice(start, i+1)).substr(1).split('%')
      for (var j = 0; j < h.length; j++)
        byteArray.push(parseInt(h[j], 16))
    }
  }
  return byteArray
}

function asciiToBytes (str) {
  var byteArray = []
  for (var i = 0; i < str.length; i++) {
    // Node's code seems to be doing this and not & 0x7F..
    byteArray.push(str.charCodeAt(i) & 0xFF)
  }
  return byteArray
}

function utf16leToBytes (str) {
  var c, hi, lo
  var byteArray = []
  for (var i = 0; i < str.length; i++) {
    c = str.charCodeAt(i)
    hi = c >> 8
    lo = c % 256
    byteArray.push(lo)
    byteArray.push(hi)
  }

  return byteArray
}

function base64ToBytes (str) {
  return base64.toByteArray(str)
}

function blitBuffer (src, dst, offset, length) {
  var pos
  for (var i = 0; i < length; i++) {
    if ((i + offset >= dst.length) || (i >= src.length))
      break
    dst[i + offset] = src[i]
  }
  return i
}

function decodeUtf8Char (str) {
  try {
    return decodeURIComponent(str)
  } catch (err) {
    return String.fromCharCode(0xFFFD) // UTF 8 invalid char
  }
}

/*
 * We have to make sure that the value is a valid integer. This means that it
 * is non-negative. It has no fractional component and that it does not
 * exceed the maximum allowed value.
 */
function verifuint (value, max) {
  assert(typeof value === 'number', 'cannot write a non-number as a number')
  assert(value >= 0, 'specified a negative value for writing an unsigned value')
  assert(value <= max, 'value is larger than maximum value for type')
  assert(Math.floor(value) === value, 'value has a fractional component')
}

function verifsint (value, max, min) {
  assert(typeof value === 'number', 'cannot write a non-number as a number')
  assert(value <= max, 'value larger than maximum allowed value')
  assert(value >= min, 'value smaller than minimum allowed value')
  assert(Math.floor(value) === value, 'value has a fractional component')
}

function verifIEEE754 (value, max, min) {
  assert(typeof value === 'number', 'cannot write a non-number as a number')
  assert(value <= max, 'value larger than maximum allowed value')
  assert(value >= min, 'value smaller than minimum allowed value')
}

function assert (test, message) {
  if (!test) throw new Error(message || 'Failed assertion')
}

},{"base64-js":2,"ieee754":3}],2:[function(require,module,exports){
var lookup = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

;(function (exports) {
	'use strict';

  var Arr = (typeof Uint8Array !== 'undefined')
    ? Uint8Array
    : Array

	var ZERO   = '0'.charCodeAt(0)
	var PLUS   = '+'.charCodeAt(0)
	var SLASH  = '/'.charCodeAt(0)
	var NUMBER = '0'.charCodeAt(0)
	var LOWER  = 'a'.charCodeAt(0)
	var UPPER  = 'A'.charCodeAt(0)

	function decode (elt) {
		var code = elt.charCodeAt(0)
		if (code === PLUS)
			return 62 // '+'
		if (code === SLASH)
			return 63 // '/'
		if (code < NUMBER)
			return -1 //no match
		if (code < NUMBER + 10)
			return code - NUMBER + 26 + 26
		if (code < UPPER + 26)
			return code - UPPER
		if (code < LOWER + 26)
			return code - LOWER + 26
	}

	function b64ToByteArray (b64) {
		var i, j, l, tmp, placeHolders, arr

		if (b64.length % 4 > 0) {
			throw new Error('Invalid string. Length must be a multiple of 4')
		}

		// the number of equal signs (place holders)
		// if there are two placeholders, than the two characters before it
		// represent one byte
		// if there is only one, then the three characters before it represent 2 bytes
		// this is just a cheap hack to not do indexOf twice
		var len = b64.length
		placeHolders = '=' === b64.charAt(len - 2) ? 2 : '=' === b64.charAt(len - 1) ? 1 : 0

		// base64 is 4/3 + up to two characters of the original data
		arr = new Arr(b64.length * 3 / 4 - placeHolders)

		// if there are placeholders, only get up to the last complete 4 chars
		l = placeHolders > 0 ? b64.length - 4 : b64.length

		var L = 0

		function push (v) {
			arr[L++] = v
		}

		for (i = 0, j = 0; i < l; i += 4, j += 3) {
			tmp = (decode(b64.charAt(i)) << 18) | (decode(b64.charAt(i + 1)) << 12) | (decode(b64.charAt(i + 2)) << 6) | decode(b64.charAt(i + 3))
			push((tmp & 0xFF0000) >> 16)
			push((tmp & 0xFF00) >> 8)
			push(tmp & 0xFF)
		}

		if (placeHolders === 2) {
			tmp = (decode(b64.charAt(i)) << 2) | (decode(b64.charAt(i + 1)) >> 4)
			push(tmp & 0xFF)
		} else if (placeHolders === 1) {
			tmp = (decode(b64.charAt(i)) << 10) | (decode(b64.charAt(i + 1)) << 4) | (decode(b64.charAt(i + 2)) >> 2)
			push((tmp >> 8) & 0xFF)
			push(tmp & 0xFF)
		}

		return arr
	}

	function uint8ToBase64 (uint8) {
		var i,
			extraBytes = uint8.length % 3, // if we have 1 byte left, pad 2 bytes
			output = "",
			temp, length

		function encode (num) {
			return lookup.charAt(num)
		}

		function tripletToBase64 (num) {
			return encode(num >> 18 & 0x3F) + encode(num >> 12 & 0x3F) + encode(num >> 6 & 0x3F) + encode(num & 0x3F)
		}

		// go through the array every three bytes, we'll deal with trailing stuff later
		for (i = 0, length = uint8.length - extraBytes; i < length; i += 3) {
			temp = (uint8[i] << 16) + (uint8[i + 1] << 8) + (uint8[i + 2])
			output += tripletToBase64(temp)
		}

		// pad the end with zeros, but make sure to not forget the extra bytes
		switch (extraBytes) {
			case 1:
				temp = uint8[uint8.length - 1]
				output += encode(temp >> 2)
				output += encode((temp << 4) & 0x3F)
				output += '=='
				break
			case 2:
				temp = (uint8[uint8.length - 2] << 8) + (uint8[uint8.length - 1])
				output += encode(temp >> 10)
				output += encode((temp >> 4) & 0x3F)
				output += encode((temp << 2) & 0x3F)
				output += '='
				break
		}

		return output
	}

	module.exports.toByteArray = b64ToByteArray
	module.exports.fromByteArray = uint8ToBase64
}())

},{}],3:[function(require,module,exports){
exports.read = function(buffer, offset, isLE, mLen, nBytes) {
  var e, m,
      eLen = nBytes * 8 - mLen - 1,
      eMax = (1 << eLen) - 1,
      eBias = eMax >> 1,
      nBits = -7,
      i = isLE ? (nBytes - 1) : 0,
      d = isLE ? -1 : 1,
      s = buffer[offset + i];

  i += d;

  e = s & ((1 << (-nBits)) - 1);
  s >>= (-nBits);
  nBits += eLen;
  for (; nBits > 0; e = e * 256 + buffer[offset + i], i += d, nBits -= 8);

  m = e & ((1 << (-nBits)) - 1);
  e >>= (-nBits);
  nBits += mLen;
  for (; nBits > 0; m = m * 256 + buffer[offset + i], i += d, nBits -= 8);

  if (e === 0) {
    e = 1 - eBias;
  } else if (e === eMax) {
    return m ? NaN : ((s ? -1 : 1) * Infinity);
  } else {
    m = m + Math.pow(2, mLen);
    e = e - eBias;
  }
  return (s ? -1 : 1) * m * Math.pow(2, e - mLen);
};

exports.write = function(buffer, value, offset, isLE, mLen, nBytes) {
  var e, m, c,
      eLen = nBytes * 8 - mLen - 1,
      eMax = (1 << eLen) - 1,
      eBias = eMax >> 1,
      rt = (mLen === 23 ? Math.pow(2, -24) - Math.pow(2, -77) : 0),
      i = isLE ? 0 : (nBytes - 1),
      d = isLE ? 1 : -1,
      s = value < 0 || (value === 0 && 1 / value < 0) ? 1 : 0;

  value = Math.abs(value);

  if (isNaN(value) || value === Infinity) {
    m = isNaN(value) ? 1 : 0;
    e = eMax;
  } else {
    e = Math.floor(Math.log(value) / Math.LN2);
    if (value * (c = Math.pow(2, -e)) < 1) {
      e--;
      c *= 2;
    }
    if (e + eBias >= 1) {
      value += rt / c;
    } else {
      value += rt * Math.pow(2, 1 - eBias);
    }
    if (value * c >= 2) {
      e++;
      c /= 2;
    }

    if (e + eBias >= eMax) {
      m = 0;
      e = eMax;
    } else if (e + eBias >= 1) {
      m = (value * c - 1) * Math.pow(2, mLen);
      e = e + eBias;
    } else {
      m = value * Math.pow(2, eBias - 1) * Math.pow(2, mLen);
      e = 0;
    }
  }

  for (; mLen >= 8; buffer[offset + i] = m & 0xff, i += d, m /= 256, mLen -= 8);

  e = (e << mLen) | m;
  eLen += mLen;
  for (; eLen > 0; buffer[offset + i] = e & 0xff, i += d, e /= 256, eLen -= 8);

  buffer[offset + i - d] |= s * 128;
};

},{}],4:[function(require,module,exports){
var Buffer = require('buffer').Buffer;
var intSize = 4;
var zeroBuffer = new Buffer(intSize); zeroBuffer.fill(0);
var chrsz = 8;

function toArray(buf, bigEndian) {
  if ((buf.length % intSize) !== 0) {
    var len = buf.length + (intSize - (buf.length % intSize));
    buf = Buffer.concat([buf, zeroBuffer], len);
  }

  var arr = [];
  var fn = bigEndian ? buf.readInt32BE : buf.readInt32LE;
  for (var i = 0; i < buf.length; i += intSize) {
    arr.push(fn.call(buf, i));
  }
  return arr;
}

function toBuffer(arr, size, bigEndian) {
  var buf = new Buffer(size);
  var fn = bigEndian ? buf.writeInt32BE : buf.writeInt32LE;
  for (var i = 0; i < arr.length; i++) {
    fn.call(buf, arr[i], i * 4, true);
  }
  return buf;
}

function hash(buf, fn, hashSize, bigEndian) {
  if (!Buffer.isBuffer(buf)) buf = new Buffer(buf);
  var arr = fn(toArray(buf, bigEndian), buf.length * chrsz);
  return toBuffer(arr, hashSize, bigEndian);
}

module.exports = { hash: hash };

},{"buffer":1}],5:[function(require,module,exports){
var Buffer = require('buffer').Buffer
var sha = require('./sha')
var sha256 = require('./sha256')
var rng = require('./rng')
var md5 = require('./md5')

var algorithms = {
  sha1: sha,
  sha256: sha256,
  md5: md5
}

var blocksize = 64
var zeroBuffer = new Buffer(blocksize); zeroBuffer.fill(0)
function hmac(fn, key, data) {
  if(!Buffer.isBuffer(key)) key = new Buffer(key)
  if(!Buffer.isBuffer(data)) data = new Buffer(data)

  if(key.length > blocksize) {
    key = fn(key)
  } else if(key.length < blocksize) {
    key = Buffer.concat([key, zeroBuffer], blocksize)
  }

  var ipad = new Buffer(blocksize), opad = new Buffer(blocksize)
  for(var i = 0; i < blocksize; i++) {
    ipad[i] = key[i] ^ 0x36
    opad[i] = key[i] ^ 0x5C
  }

  var hash = fn(Buffer.concat([ipad, data]))
  return fn(Buffer.concat([opad, hash]))
}

function hash(alg, key) {
  alg = alg || 'sha1'
  var fn = algorithms[alg]
  var bufs = []
  var length = 0
  if(!fn) error('algorithm:', alg, 'is not yet supported')
  return {
    update: function (data) {
      if(!Buffer.isBuffer(data)) data = new Buffer(data)
        
      bufs.push(data)
      length += data.length
      return this
    },
    digest: function (enc) {
      var buf = Buffer.concat(bufs)
      var r = key ? hmac(fn, key, buf) : fn(buf)
      bufs = null
      return enc ? r.toString(enc) : r
    }
  }
}

function error () {
  var m = [].slice.call(arguments).join(' ')
  throw new Error([
    m,
    'we accept pull requests',
    'http://github.com/dominictarr/crypto-browserify'
    ].join('\n'))
}

exports.createHash = function (alg) { return hash(alg) }
exports.createHmac = function (alg, key) { return hash(alg, key) }
exports.randomBytes = function(size, callback) {
  if (callback && callback.call) {
    try {
      callback.call(this, undefined, new Buffer(rng(size)))
    } catch (err) { callback(err) }
  } else {
    return new Buffer(rng(size))
  }
}

function each(a, f) {
  for(var i in a)
    f(a[i], i)
}

// the least I can do is make error messages for the rest of the node.js/crypto api.
each(['createCredentials'
, 'createCipher'
, 'createCipheriv'
, 'createDecipher'
, 'createDecipheriv'
, 'createSign'
, 'createVerify'
, 'createDiffieHellman'
, 'pbkdf2'], function (name) {
  exports[name] = function () {
    error('sorry,', name, 'is not implemented yet')
  }
})

},{"./md5":6,"./rng":7,"./sha":8,"./sha256":9,"buffer":1}],6:[function(require,module,exports){
/*
 * A JavaScript implementation of the RSA Data Security, Inc. MD5 Message
 * Digest Algorithm, as defined in RFC 1321.
 * Version 2.1 Copyright (C) Paul Johnston 1999 - 2002.
 * Other contributors: Greg Holt, Andrew Kepert, Ydnar, Lostinet
 * Distributed under the BSD License
 * See http://pajhome.org.uk/crypt/md5 for more info.
 */

var helpers = require('./helpers');

/*
 * Perform a simple self-test to see if the VM is working
 */
function md5_vm_test()
{
  return hex_md5("abc") == "900150983cd24fb0d6963f7d28e17f72";
}

/*
 * Calculate the MD5 of an array of little-endian words, and a bit length
 */
function core_md5(x, len)
{
  /* append padding */
  x[len >> 5] |= 0x80 << ((len) % 32);
  x[(((len + 64) >>> 9) << 4) + 14] = len;

  var a =  1732584193;
  var b = -271733879;
  var c = -1732584194;
  var d =  271733878;

  for(var i = 0; i < x.length; i += 16)
  {
    var olda = a;
    var oldb = b;
    var oldc = c;
    var oldd = d;

    a = md5_ff(a, b, c, d, x[i+ 0], 7 , -680876936);
    d = md5_ff(d, a, b, c, x[i+ 1], 12, -389564586);
    c = md5_ff(c, d, a, b, x[i+ 2], 17,  606105819);
    b = md5_ff(b, c, d, a, x[i+ 3], 22, -1044525330);
    a = md5_ff(a, b, c, d, x[i+ 4], 7 , -176418897);
    d = md5_ff(d, a, b, c, x[i+ 5], 12,  1200080426);
    c = md5_ff(c, d, a, b, x[i+ 6], 17, -1473231341);
    b = md5_ff(b, c, d, a, x[i+ 7], 22, -45705983);
    a = md5_ff(a, b, c, d, x[i+ 8], 7 ,  1770035416);
    d = md5_ff(d, a, b, c, x[i+ 9], 12, -1958414417);
    c = md5_ff(c, d, a, b, x[i+10], 17, -42063);
    b = md5_ff(b, c, d, a, x[i+11], 22, -1990404162);
    a = md5_ff(a, b, c, d, x[i+12], 7 ,  1804603682);
    d = md5_ff(d, a, b, c, x[i+13], 12, -40341101);
    c = md5_ff(c, d, a, b, x[i+14], 17, -1502002290);
    b = md5_ff(b, c, d, a, x[i+15], 22,  1236535329);

    a = md5_gg(a, b, c, d, x[i+ 1], 5 , -165796510);
    d = md5_gg(d, a, b, c, x[i+ 6], 9 , -1069501632);
    c = md5_gg(c, d, a, b, x[i+11], 14,  643717713);
    b = md5_gg(b, c, d, a, x[i+ 0], 20, -373897302);
    a = md5_gg(a, b, c, d, x[i+ 5], 5 , -701558691);
    d = md5_gg(d, a, b, c, x[i+10], 9 ,  38016083);
    c = md5_gg(c, d, a, b, x[i+15], 14, -660478335);
    b = md5_gg(b, c, d, a, x[i+ 4], 20, -405537848);
    a = md5_gg(a, b, c, d, x[i+ 9], 5 ,  568446438);
    d = md5_gg(d, a, b, c, x[i+14], 9 , -1019803690);
    c = md5_gg(c, d, a, b, x[i+ 3], 14, -187363961);
    b = md5_gg(b, c, d, a, x[i+ 8], 20,  1163531501);
    a = md5_gg(a, b, c, d, x[i+13], 5 , -1444681467);
    d = md5_gg(d, a, b, c, x[i+ 2], 9 , -51403784);
    c = md5_gg(c, d, a, b, x[i+ 7], 14,  1735328473);
    b = md5_gg(b, c, d, a, x[i+12], 20, -1926607734);

    a = md5_hh(a, b, c, d, x[i+ 5], 4 , -378558);
    d = md5_hh(d, a, b, c, x[i+ 8], 11, -2022574463);
    c = md5_hh(c, d, a, b, x[i+11], 16,  1839030562);
    b = md5_hh(b, c, d, a, x[i+14], 23, -35309556);
    a = md5_hh(a, b, c, d, x[i+ 1], 4 , -1530992060);
    d = md5_hh(d, a, b, c, x[i+ 4], 11,  1272893353);
    c = md5_hh(c, d, a, b, x[i+ 7], 16, -155497632);
    b = md5_hh(b, c, d, a, x[i+10], 23, -1094730640);
    a = md5_hh(a, b, c, d, x[i+13], 4 ,  681279174);
    d = md5_hh(d, a, b, c, x[i+ 0], 11, -358537222);
    c = md5_hh(c, d, a, b, x[i+ 3], 16, -722521979);
    b = md5_hh(b, c, d, a, x[i+ 6], 23,  76029189);
    a = md5_hh(a, b, c, d, x[i+ 9], 4 , -640364487);
    d = md5_hh(d, a, b, c, x[i+12], 11, -421815835);
    c = md5_hh(c, d, a, b, x[i+15], 16,  530742520);
    b = md5_hh(b, c, d, a, x[i+ 2], 23, -995338651);

    a = md5_ii(a, b, c, d, x[i+ 0], 6 , -198630844);
    d = md5_ii(d, a, b, c, x[i+ 7], 10,  1126891415);
    c = md5_ii(c, d, a, b, x[i+14], 15, -1416354905);
    b = md5_ii(b, c, d, a, x[i+ 5], 21, -57434055);
    a = md5_ii(a, b, c, d, x[i+12], 6 ,  1700485571);
    d = md5_ii(d, a, b, c, x[i+ 3], 10, -1894986606);
    c = md5_ii(c, d, a, b, x[i+10], 15, -1051523);
    b = md5_ii(b, c, d, a, x[i+ 1], 21, -2054922799);
    a = md5_ii(a, b, c, d, x[i+ 8], 6 ,  1873313359);
    d = md5_ii(d, a, b, c, x[i+15], 10, -30611744);
    c = md5_ii(c, d, a, b, x[i+ 6], 15, -1560198380);
    b = md5_ii(b, c, d, a, x[i+13], 21,  1309151649);
    a = md5_ii(a, b, c, d, x[i+ 4], 6 , -145523070);
    d = md5_ii(d, a, b, c, x[i+11], 10, -1120210379);
    c = md5_ii(c, d, a, b, x[i+ 2], 15,  718787259);
    b = md5_ii(b, c, d, a, x[i+ 9], 21, -343485551);

    a = safe_add(a, olda);
    b = safe_add(b, oldb);
    c = safe_add(c, oldc);
    d = safe_add(d, oldd);
  }
  return Array(a, b, c, d);

}

/*
 * These functions implement the four basic operations the algorithm uses.
 */
function md5_cmn(q, a, b, x, s, t)
{
  return safe_add(bit_rol(safe_add(safe_add(a, q), safe_add(x, t)), s),b);
}
function md5_ff(a, b, c, d, x, s, t)
{
  return md5_cmn((b & c) | ((~b) & d), a, b, x, s, t);
}
function md5_gg(a, b, c, d, x, s, t)
{
  return md5_cmn((b & d) | (c & (~d)), a, b, x, s, t);
}
function md5_hh(a, b, c, d, x, s, t)
{
  return md5_cmn(b ^ c ^ d, a, b, x, s, t);
}
function md5_ii(a, b, c, d, x, s, t)
{
  return md5_cmn(c ^ (b | (~d)), a, b, x, s, t);
}

/*
 * Add integers, wrapping at 2^32. This uses 16-bit operations internally
 * to work around bugs in some JS interpreters.
 */
function safe_add(x, y)
{
  var lsw = (x & 0xFFFF) + (y & 0xFFFF);
  var msw = (x >> 16) + (y >> 16) + (lsw >> 16);
  return (msw << 16) | (lsw & 0xFFFF);
}

/*
 * Bitwise rotate a 32-bit number to the left.
 */
function bit_rol(num, cnt)
{
  return (num << cnt) | (num >>> (32 - cnt));
}

module.exports = function md5(buf) {
  return helpers.hash(buf, core_md5, 16);
};

},{"./helpers":4}],7:[function(require,module,exports){
// Original code adapted from Robert Kieffer.
// details at https://github.com/broofa/node-uuid
(function() {
  var _global = this;

  var mathRNG, whatwgRNG;

  // NOTE: Math.random() does not guarantee "cryptographic quality"
  mathRNG = function(size) {
    var bytes = new Array(size);
    var r;

    for (var i = 0, r; i < size; i++) {
      if ((i & 0x03) == 0) r = Math.random() * 0x100000000;
      bytes[i] = r >>> ((i & 0x03) << 3) & 0xff;
    }

    return bytes;
  }

  if (_global.crypto && crypto.getRandomValues) {
    whatwgRNG = function(size) {
      var bytes = new Uint8Array(size);
      crypto.getRandomValues(bytes);
      return bytes;
    }
  }

  module.exports = whatwgRNG || mathRNG;

}())

},{}],8:[function(require,module,exports){
/*
 * A JavaScript implementation of the Secure Hash Algorithm, SHA-1, as defined
 * in FIPS PUB 180-1
 * Version 2.1a Copyright Paul Johnston 2000 - 2002.
 * Other contributors: Greg Holt, Andrew Kepert, Ydnar, Lostinet
 * Distributed under the BSD License
 * See http://pajhome.org.uk/crypt/md5 for details.
 */

var helpers = require('./helpers');

/*
 * Calculate the SHA-1 of an array of big-endian words, and a bit length
 */
function core_sha1(x, len)
{
  /* append padding */
  x[len >> 5] |= 0x80 << (24 - len % 32);
  x[((len + 64 >> 9) << 4) + 15] = len;

  var w = Array(80);
  var a =  1732584193;
  var b = -271733879;
  var c = -1732584194;
  var d =  271733878;
  var e = -1009589776;

  for(var i = 0; i < x.length; i += 16)
  {
    var olda = a;
    var oldb = b;
    var oldc = c;
    var oldd = d;
    var olde = e;

    for(var j = 0; j < 80; j++)
    {
      if(j < 16) w[j] = x[i + j];
      else w[j] = rol(w[j-3] ^ w[j-8] ^ w[j-14] ^ w[j-16], 1);
      var t = safe_add(safe_add(rol(a, 5), sha1_ft(j, b, c, d)),
                       safe_add(safe_add(e, w[j]), sha1_kt(j)));
      e = d;
      d = c;
      c = rol(b, 30);
      b = a;
      a = t;
    }

    a = safe_add(a, olda);
    b = safe_add(b, oldb);
    c = safe_add(c, oldc);
    d = safe_add(d, oldd);
    e = safe_add(e, olde);
  }
  return Array(a, b, c, d, e);

}

/*
 * Perform the appropriate triplet combination function for the current
 * iteration
 */
function sha1_ft(t, b, c, d)
{
  if(t < 20) return (b & c) | ((~b) & d);
  if(t < 40) return b ^ c ^ d;
  if(t < 60) return (b & c) | (b & d) | (c & d);
  return b ^ c ^ d;
}

/*
 * Determine the appropriate additive constant for the current iteration
 */
function sha1_kt(t)
{
  return (t < 20) ?  1518500249 : (t < 40) ?  1859775393 :
         (t < 60) ? -1894007588 : -899497514;
}

/*
 * Add integers, wrapping at 2^32. This uses 16-bit operations internally
 * to work around bugs in some JS interpreters.
 */
function safe_add(x, y)
{
  var lsw = (x & 0xFFFF) + (y & 0xFFFF);
  var msw = (x >> 16) + (y >> 16) + (lsw >> 16);
  return (msw << 16) | (lsw & 0xFFFF);
}

/*
 * Bitwise rotate a 32-bit number to the left.
 */
function rol(num, cnt)
{
  return (num << cnt) | (num >>> (32 - cnt));
}

module.exports = function sha1(buf) {
  return helpers.hash(buf, core_sha1, 20, true);
};

},{"./helpers":4}],9:[function(require,module,exports){

/**
 * A JavaScript implementation of the Secure Hash Algorithm, SHA-256, as defined
 * in FIPS 180-2
 * Version 2.2-beta Copyright Angel Marin, Paul Johnston 2000 - 2009.
 * Other contributors: Greg Holt, Andrew Kepert, Ydnar, Lostinet
 *
 */

var helpers = require('./helpers');

var safe_add = function(x, y) {
  var lsw = (x & 0xFFFF) + (y & 0xFFFF);
  var msw = (x >> 16) + (y >> 16) + (lsw >> 16);
  return (msw << 16) | (lsw & 0xFFFF);
};

var S = function(X, n) {
  return (X >>> n) | (X << (32 - n));
};

var R = function(X, n) {
  return (X >>> n);
};

var Ch = function(x, y, z) {
  return ((x & y) ^ ((~x) & z));
};

var Maj = function(x, y, z) {
  return ((x & y) ^ (x & z) ^ (y & z));
};

var Sigma0256 = function(x) {
  return (S(x, 2) ^ S(x, 13) ^ S(x, 22));
};

var Sigma1256 = function(x) {
  return (S(x, 6) ^ S(x, 11) ^ S(x, 25));
};

var Gamma0256 = function(x) {
  return (S(x, 7) ^ S(x, 18) ^ R(x, 3));
};

var Gamma1256 = function(x) {
  return (S(x, 17) ^ S(x, 19) ^ R(x, 10));
};

var core_sha256 = function(m, l) {
  var K = new Array(0x428A2F98,0x71374491,0xB5C0FBCF,0xE9B5DBA5,0x3956C25B,0x59F111F1,0x923F82A4,0xAB1C5ED5,0xD807AA98,0x12835B01,0x243185BE,0x550C7DC3,0x72BE5D74,0x80DEB1FE,0x9BDC06A7,0xC19BF174,0xE49B69C1,0xEFBE4786,0xFC19DC6,0x240CA1CC,0x2DE92C6F,0x4A7484AA,0x5CB0A9DC,0x76F988DA,0x983E5152,0xA831C66D,0xB00327C8,0xBF597FC7,0xC6E00BF3,0xD5A79147,0x6CA6351,0x14292967,0x27B70A85,0x2E1B2138,0x4D2C6DFC,0x53380D13,0x650A7354,0x766A0ABB,0x81C2C92E,0x92722C85,0xA2BFE8A1,0xA81A664B,0xC24B8B70,0xC76C51A3,0xD192E819,0xD6990624,0xF40E3585,0x106AA070,0x19A4C116,0x1E376C08,0x2748774C,0x34B0BCB5,0x391C0CB3,0x4ED8AA4A,0x5B9CCA4F,0x682E6FF3,0x748F82EE,0x78A5636F,0x84C87814,0x8CC70208,0x90BEFFFA,0xA4506CEB,0xBEF9A3F7,0xC67178F2);
  var HASH = new Array(0x6A09E667, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A, 0x510E527F, 0x9B05688C, 0x1F83D9AB, 0x5BE0CD19);
    var W = new Array(64);
    var a, b, c, d, e, f, g, h, i, j;
    var T1, T2;
  /* append padding */
  m[l >> 5] |= 0x80 << (24 - l % 32);
  m[((l + 64 >> 9) << 4) + 15] = l;
  for (var i = 0; i < m.length; i += 16) {
    a = HASH[0]; b = HASH[1]; c = HASH[2]; d = HASH[3]; e = HASH[4]; f = HASH[5]; g = HASH[6]; h = HASH[7];
    for (var j = 0; j < 64; j++) {
      if (j < 16) {
        W[j] = m[j + i];
      } else {
        W[j] = safe_add(safe_add(safe_add(Gamma1256(W[j - 2]), W[j - 7]), Gamma0256(W[j - 15])), W[j - 16]);
      }
      T1 = safe_add(safe_add(safe_add(safe_add(h, Sigma1256(e)), Ch(e, f, g)), K[j]), W[j]);
      T2 = safe_add(Sigma0256(a), Maj(a, b, c));
      h = g; g = f; f = e; e = safe_add(d, T1); d = c; c = b; b = a; a = safe_add(T1, T2);
    }
    HASH[0] = safe_add(a, HASH[0]); HASH[1] = safe_add(b, HASH[1]); HASH[2] = safe_add(c, HASH[2]); HASH[3] = safe_add(d, HASH[3]);
    HASH[4] = safe_add(e, HASH[4]); HASH[5] = safe_add(f, HASH[5]); HASH[6] = safe_add(g, HASH[6]); HASH[7] = safe_add(h, HASH[7]);
  }
  return HASH;
};

module.exports = function sha256(buf) {
  return helpers.hash(buf, core_sha256, 32, true);
};

},{"./helpers":4}],10:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

function EventEmitter() {
  this._events = this._events || {};
  this._maxListeners = this._maxListeners || undefined;
}
module.exports = EventEmitter;

// Backwards-compat with node 0.10.x
EventEmitter.EventEmitter = EventEmitter;

EventEmitter.prototype._events = undefined;
EventEmitter.prototype._maxListeners = undefined;

// By default EventEmitters will print a warning if more than 10 listeners are
// added to it. This is a useful default which helps finding memory leaks.
EventEmitter.defaultMaxListeners = 10;

// Obviously not all Emitters should be limited to 10. This function allows
// that to be increased. Set to zero for unlimited.
EventEmitter.prototype.setMaxListeners = function(n) {
  if (!isNumber(n) || n < 0 || isNaN(n))
    throw TypeError('n must be a positive number');
  this._maxListeners = n;
  return this;
};

EventEmitter.prototype.emit = function(type) {
  var er, handler, len, args, i, listeners;

  if (!this._events)
    this._events = {};

  // If there is no 'error' event listener then throw.
  if (type === 'error') {
    if (!this._events.error ||
        (isObject(this._events.error) && !this._events.error.length)) {
      er = arguments[1];
      if (er instanceof Error) {
        throw er; // Unhandled 'error' event
      } else {
        throw TypeError('Uncaught, unspecified "error" event.');
      }
      return false;
    }
  }

  handler = this._events[type];

  if (isUndefined(handler))
    return false;

  if (isFunction(handler)) {
    switch (arguments.length) {
      // fast cases
      case 1:
        handler.call(this);
        break;
      case 2:
        handler.call(this, arguments[1]);
        break;
      case 3:
        handler.call(this, arguments[1], arguments[2]);
        break;
      // slower
      default:
        len = arguments.length;
        args = new Array(len - 1);
        for (i = 1; i < len; i++)
          args[i - 1] = arguments[i];
        handler.apply(this, args);
    }
  } else if (isObject(handler)) {
    len = arguments.length;
    args = new Array(len - 1);
    for (i = 1; i < len; i++)
      args[i - 1] = arguments[i];

    listeners = handler.slice();
    len = listeners.length;
    for (i = 0; i < len; i++)
      listeners[i].apply(this, args);
  }

  return true;
};

EventEmitter.prototype.addListener = function(type, listener) {
  var m;

  if (!isFunction(listener))
    throw TypeError('listener must be a function');

  if (!this._events)
    this._events = {};

  // To avoid recursion in the case that type === "newListener"! Before
  // adding it to the listeners, first emit "newListener".
  if (this._events.newListener)
    this.emit('newListener', type,
              isFunction(listener.listener) ?
              listener.listener : listener);

  if (!this._events[type])
    // Optimize the case of one listener. Don't need the extra array object.
    this._events[type] = listener;
  else if (isObject(this._events[type]))
    // If we've already got an array, just append.
    this._events[type].push(listener);
  else
    // Adding the second element, need to change to array.
    this._events[type] = [this._events[type], listener];

  // Check for listener leak
  if (isObject(this._events[type]) && !this._events[type].warned) {
    var m;
    if (!isUndefined(this._maxListeners)) {
      m = this._maxListeners;
    } else {
      m = EventEmitter.defaultMaxListeners;
    }

    if (m && m > 0 && this._events[type].length > m) {
      this._events[type].warned = true;
      console.error('(node) warning: possible EventEmitter memory ' +
                    'leak detected. %d listeners added. ' +
                    'Use emitter.setMaxListeners() to increase limit.',
                    this._events[type].length);
      console.trace();
    }
  }

  return this;
};

EventEmitter.prototype.on = EventEmitter.prototype.addListener;

EventEmitter.prototype.once = function(type, listener) {
  if (!isFunction(listener))
    throw TypeError('listener must be a function');

  var fired = false;

  function g() {
    this.removeListener(type, g);

    if (!fired) {
      fired = true;
      listener.apply(this, arguments);
    }
  }

  g.listener = listener;
  this.on(type, g);

  return this;
};

// emits a 'removeListener' event iff the listener was removed
EventEmitter.prototype.removeListener = function(type, listener) {
  var list, position, length, i;

  if (!isFunction(listener))
    throw TypeError('listener must be a function');

  if (!this._events || !this._events[type])
    return this;

  list = this._events[type];
  length = list.length;
  position = -1;

  if (list === listener ||
      (isFunction(list.listener) && list.listener === listener)) {
    delete this._events[type];
    if (this._events.removeListener)
      this.emit('removeListener', type, listener);

  } else if (isObject(list)) {
    for (i = length; i-- > 0;) {
      if (list[i] === listener ||
          (list[i].listener && list[i].listener === listener)) {
        position = i;
        break;
      }
    }

    if (position < 0)
      return this;

    if (list.length === 1) {
      list.length = 0;
      delete this._events[type];
    } else {
      list.splice(position, 1);
    }

    if (this._events.removeListener)
      this.emit('removeListener', type, listener);
  }

  return this;
};

EventEmitter.prototype.removeAllListeners = function(type) {
  var key, listeners;

  if (!this._events)
    return this;

  // not listening for removeListener, no need to emit
  if (!this._events.removeListener) {
    if (arguments.length === 0)
      this._events = {};
    else if (this._events[type])
      delete this._events[type];
    return this;
  }

  // emit removeListener for all listeners on all events
  if (arguments.length === 0) {
    for (key in this._events) {
      if (key === 'removeListener') continue;
      this.removeAllListeners(key);
    }
    this.removeAllListeners('removeListener');
    this._events = {};
    return this;
  }

  listeners = this._events[type];

  if (isFunction(listeners)) {
    this.removeListener(type, listeners);
  } else {
    // LIFO order
    while (listeners.length)
      this.removeListener(type, listeners[listeners.length - 1]);
  }
  delete this._events[type];

  return this;
};

EventEmitter.prototype.listeners = function(type) {
  var ret;
  if (!this._events || !this._events[type])
    ret = [];
  else if (isFunction(this._events[type]))
    ret = [this._events[type]];
  else
    ret = this._events[type].slice();
  return ret;
};

EventEmitter.listenerCount = function(emitter, type) {
  var ret;
  if (!emitter._events || !emitter._events[type])
    ret = 0;
  else if (isFunction(emitter._events[type]))
    ret = 1;
  else
    ret = emitter._events[type].length;
  return ret;
};

function isFunction(arg) {
  return typeof arg === 'function';
}

function isNumber(arg) {
  return typeof arg === 'number';
}

function isObject(arg) {
  return typeof arg === 'object' && arg !== null;
}

function isUndefined(arg) {
  return arg === void 0;
}

},{}],11:[function(require,module,exports){
if (typeof Object.create === 'function') {
  // implementation from standard node.js 'util' module
  module.exports = function inherits(ctor, superCtor) {
    ctor.super_ = superCtor
    ctor.prototype = Object.create(superCtor.prototype, {
      constructor: {
        value: ctor,
        enumerable: false,
        writable: true,
        configurable: true
      }
    });
  };
} else {
  // old school shim for old browsers
  module.exports = function inherits(ctor, superCtor) {
    ctor.super_ = superCtor
    var TempCtor = function () {}
    TempCtor.prototype = superCtor.prototype
    ctor.prototype = new TempCtor()
    ctor.prototype.constructor = ctor
  }
}

},{}],12:[function(require,module,exports){
// shim for using process in browser

var process = module.exports = {};

process.nextTick = (function () {
    var canSetImmediate = typeof window !== 'undefined'
    && window.setImmediate;
    var canPost = typeof window !== 'undefined'
    && window.postMessage && window.addEventListener
    ;

    if (canSetImmediate) {
        return function (f) { return window.setImmediate(f) };
    }

    if (canPost) {
        var queue = [];
        window.addEventListener('message', function (ev) {
            var source = ev.source;
            if ((source === window || source === null) && ev.data === 'process-tick') {
                ev.stopPropagation();
                if (queue.length > 0) {
                    var fn = queue.shift();
                    fn();
                }
            }
        }, true);

        return function nextTick(fn) {
            queue.push(fn);
            window.postMessage('process-tick', '*');
        };
    }

    return function nextTick(fn) {
        setTimeout(fn, 0);
    };
})();

process.title = 'browser';
process.browser = true;
process.env = {};
process.argv = [];

function noop() {}

process.on = noop;
process.once = noop;
process.off = noop;
process.emit = noop;

process.binding = function (name) {
    throw new Error('process.binding is not supported');
}

// TODO(shtylman)
process.cwd = function () { return '/' };
process.chdir = function (dir) {
    throw new Error('process.chdir is not supported');
};

},{}],13:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

// a duplex stream is just a stream that is both readable and writable.
// Since JS doesn't have multiple prototypal inheritance, this class
// prototypally inherits from Readable, and then parasitically from
// Writable.

module.exports = Duplex;
var inherits = require('inherits');
var setImmediate = require('process/browser.js').nextTick;
var Readable = require('./readable.js');
var Writable = require('./writable.js');

inherits(Duplex, Readable);

Duplex.prototype.write = Writable.prototype.write;
Duplex.prototype.end = Writable.prototype.end;
Duplex.prototype._write = Writable.prototype._write;

function Duplex(options) {
  if (!(this instanceof Duplex))
    return new Duplex(options);

  Readable.call(this, options);
  Writable.call(this, options);

  if (options && options.readable === false)
    this.readable = false;

  if (options && options.writable === false)
    this.writable = false;

  this.allowHalfOpen = true;
  if (options && options.allowHalfOpen === false)
    this.allowHalfOpen = false;

  this.once('end', onend);
}

// the no-half-open enforcer
function onend() {
  // if we allow half-open state, or if the writable side ended,
  // then we're ok.
  if (this.allowHalfOpen || this._writableState.ended)
    return;

  // no more data can be written.
  // But allow more writes to happen in this tick.
  var self = this;
  setImmediate(function () {
    self.end();
  });
}

},{"./readable.js":17,"./writable.js":19,"inherits":11,"process/browser.js":15}],14:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

module.exports = Stream;

var EE = require('events').EventEmitter;
var inherits = require('inherits');

inherits(Stream, EE);
Stream.Readable = require('./readable.js');
Stream.Writable = require('./writable.js');
Stream.Duplex = require('./duplex.js');
Stream.Transform = require('./transform.js');
Stream.PassThrough = require('./passthrough.js');

// Backwards-compat with node 0.4.x
Stream.Stream = Stream;



// old-style streams.  Note that the pipe method (the only relevant
// part of this class) is overridden in the Readable class.

function Stream() {
  EE.call(this);
}

Stream.prototype.pipe = function(dest, options) {
  var source = this;

  function ondata(chunk) {
    if (dest.writable) {
      if (false === dest.write(chunk) && source.pause) {
        source.pause();
      }
    }
  }

  source.on('data', ondata);

  function ondrain() {
    if (source.readable && source.resume) {
      source.resume();
    }
  }

  dest.on('drain', ondrain);

  // If the 'end' option is not supplied, dest.end() will be called when
  // source gets the 'end' or 'close' events.  Only dest.end() once.
  if (!dest._isStdio && (!options || options.end !== false)) {
    source.on('end', onend);
    source.on('close', onclose);
  }

  var didOnEnd = false;
  function onend() {
    if (didOnEnd) return;
    didOnEnd = true;

    dest.end();
  }


  function onclose() {
    if (didOnEnd) return;
    didOnEnd = true;

    if (typeof dest.destroy === 'function') dest.destroy();
  }

  // don't leave dangling pipes when there are errors.
  function onerror(er) {
    cleanup();
    if (EE.listenerCount(this, 'error') === 0) {
      throw er; // Unhandled stream error in pipe.
    }
  }

  source.on('error', onerror);
  dest.on('error', onerror);

  // remove all the event listeners that were added.
  function cleanup() {
    source.removeListener('data', ondata);
    dest.removeListener('drain', ondrain);

    source.removeListener('end', onend);
    source.removeListener('close', onclose);

    source.removeListener('error', onerror);
    dest.removeListener('error', onerror);

    source.removeListener('end', cleanup);
    source.removeListener('close', cleanup);

    dest.removeListener('close', cleanup);
  }

  source.on('end', cleanup);
  source.on('close', cleanup);

  dest.on('close', cleanup);

  dest.emit('pipe', source);

  // Allow for unix-like usage: A.pipe(B).pipe(C)
  return dest;
};

},{"./duplex.js":13,"./passthrough.js":16,"./readable.js":17,"./transform.js":18,"./writable.js":19,"events":10,"inherits":11}],15:[function(require,module,exports){
// shim for using process in browser

var process = module.exports = {};

process.nextTick = (function () {
    var canSetImmediate = typeof window !== 'undefined'
    && window.setImmediate;
    var canPost = typeof window !== 'undefined'
    && window.postMessage && window.addEventListener
    ;

    if (canSetImmediate) {
        return function (f) { return window.setImmediate(f) };
    }

    if (canPost) {
        var queue = [];
        window.addEventListener('message', function (ev) {
            var source = ev.source;
            if ((source === window || source === null) && ev.data === 'process-tick') {
                ev.stopPropagation();
                if (queue.length > 0) {
                    var fn = queue.shift();
                    fn();
                }
            }
        }, true);

        return function nextTick(fn) {
            queue.push(fn);
            window.postMessage('process-tick', '*');
        };
    }

    return function nextTick(fn) {
        setTimeout(fn, 0);
    };
})();

process.title = 'browser';
process.browser = true;
process.env = {};
process.argv = [];

process.binding = function (name) {
    throw new Error('process.binding is not supported');
}

// TODO(shtylman)
process.cwd = function () { return '/' };
process.chdir = function (dir) {
    throw new Error('process.chdir is not supported');
};

},{}],16:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

// a passthrough stream.
// basically just the most minimal sort of Transform stream.
// Every written chunk gets output as-is.

module.exports = PassThrough;

var Transform = require('./transform.js');
var inherits = require('inherits');
inherits(PassThrough, Transform);

function PassThrough(options) {
  if (!(this instanceof PassThrough))
    return new PassThrough(options);

  Transform.call(this, options);
}

PassThrough.prototype._transform = function(chunk, encoding, cb) {
  cb(null, chunk);
};

},{"./transform.js":18,"inherits":11}],17:[function(require,module,exports){
(function (process){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

module.exports = Readable;
Readable.ReadableState = ReadableState;

var EE = require('events').EventEmitter;
var Stream = require('./index.js');
var Buffer = require('buffer').Buffer;
var setImmediate = require('process/browser.js').nextTick;
var StringDecoder;

var inherits = require('inherits');
inherits(Readable, Stream);

function ReadableState(options, stream) {
  options = options || {};

  // the point at which it stops calling _read() to fill the buffer
  // Note: 0 is a valid value, means "don't call _read preemptively ever"
  var hwm = options.highWaterMark;
  this.highWaterMark = (hwm || hwm === 0) ? hwm : 16 * 1024;

  // cast to ints.
  this.highWaterMark = ~~this.highWaterMark;

  this.buffer = [];
  this.length = 0;
  this.pipes = null;
  this.pipesCount = 0;
  this.flowing = false;
  this.ended = false;
  this.endEmitted = false;
  this.reading = false;

  // In streams that never have any data, and do push(null) right away,
  // the consumer can miss the 'end' event if they do some I/O before
  // consuming the stream.  So, we don't emit('end') until some reading
  // happens.
  this.calledRead = false;

  // a flag to be able to tell if the onwrite cb is called immediately,
  // or on a later tick.  We set this to true at first, becuase any
  // actions that shouldn't happen until "later" should generally also
  // not happen before the first write call.
  this.sync = true;

  // whenever we return null, then we set a flag to say
  // that we're awaiting a 'readable' event emission.
  this.needReadable = false;
  this.emittedReadable = false;
  this.readableListening = false;


  // object stream flag. Used to make read(n) ignore n and to
  // make all the buffer merging and length checks go away
  this.objectMode = !!options.objectMode;

  // Crypto is kind of old and crusty.  Historically, its default string
  // encoding is 'binary' so we have to make this configurable.
  // Everything else in the universe uses 'utf8', though.
  this.defaultEncoding = options.defaultEncoding || 'utf8';

  // when piping, we only care about 'readable' events that happen
  // after read()ing all the bytes and not getting any pushback.
  this.ranOut = false;

  // the number of writers that are awaiting a drain event in .pipe()s
  this.awaitDrain = 0;

  // if true, a maybeReadMore has been scheduled
  this.readingMore = false;

  this.decoder = null;
  this.encoding = null;
  if (options.encoding) {
    if (!StringDecoder)
      StringDecoder = require('string_decoder').StringDecoder;
    this.decoder = new StringDecoder(options.encoding);
    this.encoding = options.encoding;
  }
}

function Readable(options) {
  if (!(this instanceof Readable))
    return new Readable(options);

  this._readableState = new ReadableState(options, this);

  // legacy
  this.readable = true;

  Stream.call(this);
}

// Manually shove something into the read() buffer.
// This returns true if the highWaterMark has not been hit yet,
// similar to how Writable.write() returns true if you should
// write() some more.
Readable.prototype.push = function(chunk, encoding) {
  var state = this._readableState;

  if (typeof chunk === 'string' && !state.objectMode) {
    encoding = encoding || state.defaultEncoding;
    if (encoding !== state.encoding) {
      chunk = new Buffer(chunk, encoding);
      encoding = '';
    }
  }

  return readableAddChunk(this, state, chunk, encoding, false);
};

// Unshift should *always* be something directly out of read()
Readable.prototype.unshift = function(chunk) {
  var state = this._readableState;
  return readableAddChunk(this, state, chunk, '', true);
};

function readableAddChunk(stream, state, chunk, encoding, addToFront) {
  var er = chunkInvalid(state, chunk);
  if (er) {
    stream.emit('error', er);
  } else if (chunk === null || chunk === undefined) {
    state.reading = false;
    if (!state.ended)
      onEofChunk(stream, state);
  } else if (state.objectMode || chunk && chunk.length > 0) {
    if (state.ended && !addToFront) {
      var e = new Error('stream.push() after EOF');
      stream.emit('error', e);
    } else if (state.endEmitted && addToFront) {
      var e = new Error('stream.unshift() after end event');
      stream.emit('error', e);
    } else {
      if (state.decoder && !addToFront && !encoding)
        chunk = state.decoder.write(chunk);

      // update the buffer info.
      state.length += state.objectMode ? 1 : chunk.length;
      if (addToFront) {
        state.buffer.unshift(chunk);
      } else {
        state.reading = false;
        state.buffer.push(chunk);
      }

      if (state.needReadable)
        emitReadable(stream);

      maybeReadMore(stream, state);
    }
  } else if (!addToFront) {
    state.reading = false;
  }

  return needMoreData(state);
}



// if it's past the high water mark, we can push in some more.
// Also, if we have no data yet, we can stand some
// more bytes.  This is to work around cases where hwm=0,
// such as the repl.  Also, if the push() triggered a
// readable event, and the user called read(largeNumber) such that
// needReadable was set, then we ought to push more, so that another
// 'readable' event will be triggered.
function needMoreData(state) {
  return !state.ended &&
         (state.needReadable ||
          state.length < state.highWaterMark ||
          state.length === 0);
}

// backwards compatibility.
Readable.prototype.setEncoding = function(enc) {
  if (!StringDecoder)
    StringDecoder = require('string_decoder').StringDecoder;
  this._readableState.decoder = new StringDecoder(enc);
  this._readableState.encoding = enc;
};

// Don't raise the hwm > 128MB
var MAX_HWM = 0x800000;
function roundUpToNextPowerOf2(n) {
  if (n >= MAX_HWM) {
    n = MAX_HWM;
  } else {
    // Get the next highest power of 2
    n--;
    for (var p = 1; p < 32; p <<= 1) n |= n >> p;
    n++;
  }
  return n;
}

function howMuchToRead(n, state) {
  if (state.length === 0 && state.ended)
    return 0;

  if (state.objectMode)
    return n === 0 ? 0 : 1;

  if (isNaN(n) || n === null) {
    // only flow one buffer at a time
    if (state.flowing && state.buffer.length)
      return state.buffer[0].length;
    else
      return state.length;
  }

  if (n <= 0)
    return 0;

  // If we're asking for more than the target buffer level,
  // then raise the water mark.  Bump up to the next highest
  // power of 2, to prevent increasing it excessively in tiny
  // amounts.
  if (n > state.highWaterMark)
    state.highWaterMark = roundUpToNextPowerOf2(n);

  // don't have that much.  return null, unless we've ended.
  if (n > state.length) {
    if (!state.ended) {
      state.needReadable = true;
      return 0;
    } else
      return state.length;
  }

  return n;
}

// you can override either this method, or the async _read(n) below.
Readable.prototype.read = function(n) {
  var state = this._readableState;
  state.calledRead = true;
  var nOrig = n;

  if (typeof n !== 'number' || n > 0)
    state.emittedReadable = false;

  // if we're doing read(0) to trigger a readable event, but we
  // already have a bunch of data in the buffer, then just trigger
  // the 'readable' event and move on.
  if (n === 0 &&
      state.needReadable &&
      (state.length >= state.highWaterMark || state.ended)) {
    emitReadable(this);
    return null;
  }

  n = howMuchToRead(n, state);

  // if we've ended, and we're now clear, then finish it up.
  if (n === 0 && state.ended) {
    if (state.length === 0)
      endReadable(this);
    return null;
  }

  // All the actual chunk generation logic needs to be
  // *below* the call to _read.  The reason is that in certain
  // synthetic stream cases, such as passthrough streams, _read
  // may be a completely synchronous operation which may change
  // the state of the read buffer, providing enough data when
  // before there was *not* enough.
  //
  // So, the steps are:
  // 1. Figure out what the state of things will be after we do
  // a read from the buffer.
  //
  // 2. If that resulting state will trigger a _read, then call _read.
  // Note that this may be asynchronous, or synchronous.  Yes, it is
  // deeply ugly to write APIs this way, but that still doesn't mean
  // that the Readable class should behave improperly, as streams are
  // designed to be sync/async agnostic.
  // Take note if the _read call is sync or async (ie, if the read call
  // has returned yet), so that we know whether or not it's safe to emit
  // 'readable' etc.
  //
  // 3. Actually pull the requested chunks out of the buffer and return.

  // if we need a readable event, then we need to do some reading.
  var doRead = state.needReadable;

  // if we currently have less than the highWaterMark, then also read some
  if (state.length - n <= state.highWaterMark)
    doRead = true;

  // however, if we've ended, then there's no point, and if we're already
  // reading, then it's unnecessary.
  if (state.ended || state.reading)
    doRead = false;

  if (doRead) {
    state.reading = true;
    state.sync = true;
    // if the length is currently zero, then we *need* a readable event.
    if (state.length === 0)
      state.needReadable = true;
    // call internal read method
    this._read(state.highWaterMark);
    state.sync = false;
  }

  // If _read called its callback synchronously, then `reading`
  // will be false, and we need to re-evaluate how much data we
  // can return to the user.
  if (doRead && !state.reading)
    n = howMuchToRead(nOrig, state);

  var ret;
  if (n > 0)
    ret = fromList(n, state);
  else
    ret = null;

  if (ret === null) {
    state.needReadable = true;
    n = 0;
  }

  state.length -= n;

  // If we have nothing in the buffer, then we want to know
  // as soon as we *do* get something into the buffer.
  if (state.length === 0 && !state.ended)
    state.needReadable = true;

  // If we happened to read() exactly the remaining amount in the
  // buffer, and the EOF has been seen at this point, then make sure
  // that we emit 'end' on the very next tick.
  if (state.ended && !state.endEmitted && state.length === 0)
    endReadable(this);

  return ret;
};

function chunkInvalid(state, chunk) {
  var er = null;
  if (!Buffer.isBuffer(chunk) &&
      'string' !== typeof chunk &&
      chunk !== null &&
      chunk !== undefined &&
      !state.objectMode &&
      !er) {
    er = new TypeError('Invalid non-string/buffer chunk');
  }
  return er;
}


function onEofChunk(stream, state) {
  if (state.decoder && !state.ended) {
    var chunk = state.decoder.end();
    if (chunk && chunk.length) {
      state.buffer.push(chunk);
      state.length += state.objectMode ? 1 : chunk.length;
    }
  }
  state.ended = true;

  // if we've ended and we have some data left, then emit
  // 'readable' now to make sure it gets picked up.
  if (state.length > 0)
    emitReadable(stream);
  else
    endReadable(stream);
}

// Don't emit readable right away in sync mode, because this can trigger
// another read() call => stack overflow.  This way, it might trigger
// a nextTick recursion warning, but that's not so bad.
function emitReadable(stream) {
  var state = stream._readableState;
  state.needReadable = false;
  if (state.emittedReadable)
    return;

  state.emittedReadable = true;
  if (state.sync)
    setImmediate(function() {
      emitReadable_(stream);
    });
  else
    emitReadable_(stream);
}

function emitReadable_(stream) {
  stream.emit('readable');
}


// at this point, the user has presumably seen the 'readable' event,
// and called read() to consume some data.  that may have triggered
// in turn another _read(n) call, in which case reading = true if
// it's in progress.
// However, if we're not ended, or reading, and the length < hwm,
// then go ahead and try to read some more preemptively.
function maybeReadMore(stream, state) {
  if (!state.readingMore) {
    state.readingMore = true;
    setImmediate(function() {
      maybeReadMore_(stream, state);
    });
  }
}

function maybeReadMore_(stream, state) {
  var len = state.length;
  while (!state.reading && !state.flowing && !state.ended &&
         state.length < state.highWaterMark) {
    stream.read(0);
    if (len === state.length)
      // didn't get any data, stop spinning.
      break;
    else
      len = state.length;
  }
  state.readingMore = false;
}

// abstract method.  to be overridden in specific implementation classes.
// call cb(er, data) where data is <= n in length.
// for virtual (non-string, non-buffer) streams, "length" is somewhat
// arbitrary, and perhaps not very meaningful.
Readable.prototype._read = function(n) {
  this.emit('error', new Error('not implemented'));
};

Readable.prototype.pipe = function(dest, pipeOpts) {
  var src = this;
  var state = this._readableState;

  switch (state.pipesCount) {
    case 0:
      state.pipes = dest;
      break;
    case 1:
      state.pipes = [state.pipes, dest];
      break;
    default:
      state.pipes.push(dest);
      break;
  }
  state.pipesCount += 1;

  var doEnd = (!pipeOpts || pipeOpts.end !== false) &&
              dest !== process.stdout &&
              dest !== process.stderr;

  var endFn = doEnd ? onend : cleanup;
  if (state.endEmitted)
    setImmediate(endFn);
  else
    src.once('end', endFn);

  dest.on('unpipe', onunpipe);
  function onunpipe(readable) {
    if (readable !== src) return;
    cleanup();
  }

  function onend() {
    dest.end();
  }

  // when the dest drains, it reduces the awaitDrain counter
  // on the source.  This would be more elegant with a .once()
  // handler in flow(), but adding and removing repeatedly is
  // too slow.
  var ondrain = pipeOnDrain(src);
  dest.on('drain', ondrain);

  function cleanup() {
    // cleanup event handlers once the pipe is broken
    dest.removeListener('close', onclose);
    dest.removeListener('finish', onfinish);
    dest.removeListener('drain', ondrain);
    dest.removeListener('error', onerror);
    dest.removeListener('unpipe', onunpipe);
    src.removeListener('end', onend);
    src.removeListener('end', cleanup);

    // if the reader is waiting for a drain event from this
    // specific writer, then it would cause it to never start
    // flowing again.
    // So, if this is awaiting a drain, then we just call it now.
    // If we don't know, then assume that we are waiting for one.
    if (!dest._writableState || dest._writableState.needDrain)
      ondrain();
  }

  // if the dest has an error, then stop piping into it.
  // however, don't suppress the throwing behavior for this.
  // check for listeners before emit removes one-time listeners.
  var errListeners = EE.listenerCount(dest, 'error');
  function onerror(er) {
    unpipe();
    if (errListeners === 0 && EE.listenerCount(dest, 'error') === 0)
      dest.emit('error', er);
  }
  dest.once('error', onerror);

  // Both close and finish should trigger unpipe, but only once.
  function onclose() {
    dest.removeListener('finish', onfinish);
    unpipe();
  }
  dest.once('close', onclose);
  function onfinish() {
    dest.removeListener('close', onclose);
    unpipe();
  }
  dest.once('finish', onfinish);

  function unpipe() {
    src.unpipe(dest);
  }

  // tell the dest that it's being piped to
  dest.emit('pipe', src);

  // start the flow if it hasn't been started already.
  if (!state.flowing) {
    // the handler that waits for readable events after all
    // the data gets sucked out in flow.
    // This would be easier to follow with a .once() handler
    // in flow(), but that is too slow.
    this.on('readable', pipeOnReadable);

    state.flowing = true;
    setImmediate(function() {
      flow(src);
    });
  }

  return dest;
};

function pipeOnDrain(src) {
  return function() {
    var dest = this;
    var state = src._readableState;
    state.awaitDrain--;
    if (state.awaitDrain === 0)
      flow(src);
  };
}

function flow(src) {
  var state = src._readableState;
  var chunk;
  state.awaitDrain = 0;

  function write(dest, i, list) {
    var written = dest.write(chunk);
    if (false === written) {
      state.awaitDrain++;
    }
  }

  while (state.pipesCount && null !== (chunk = src.read())) {

    if (state.pipesCount === 1)
      write(state.pipes, 0, null);
    else
      forEach(state.pipes, write);

    src.emit('data', chunk);

    // if anyone needs a drain, then we have to wait for that.
    if (state.awaitDrain > 0)
      return;
  }

  // if every destination was unpiped, either before entering this
  // function, or in the while loop, then stop flowing.
  //
  // NB: This is a pretty rare edge case.
  if (state.pipesCount === 0) {
    state.flowing = false;

    // if there were data event listeners added, then switch to old mode.
    if (EE.listenerCount(src, 'data') > 0)
      emitDataEvents(src);
    return;
  }

  // at this point, no one needed a drain, so we just ran out of data
  // on the next readable event, start it over again.
  state.ranOut = true;
}

function pipeOnReadable() {
  if (this._readableState.ranOut) {
    this._readableState.ranOut = false;
    flow(this);
  }
}


Readable.prototype.unpipe = function(dest) {
  var state = this._readableState;

  // if we're not piping anywhere, then do nothing.
  if (state.pipesCount === 0)
    return this;

  // just one destination.  most common case.
  if (state.pipesCount === 1) {
    // passed in one, but it's not the right one.
    if (dest && dest !== state.pipes)
      return this;

    if (!dest)
      dest = state.pipes;

    // got a match.
    state.pipes = null;
    state.pipesCount = 0;
    this.removeListener('readable', pipeOnReadable);
    state.flowing = false;
    if (dest)
      dest.emit('unpipe', this);
    return this;
  }

  // slow case. multiple pipe destinations.

  if (!dest) {
    // remove all.
    var dests = state.pipes;
    var len = state.pipesCount;
    state.pipes = null;
    state.pipesCount = 0;
    this.removeListener('readable', pipeOnReadable);
    state.flowing = false;

    for (var i = 0; i < len; i++)
      dests[i].emit('unpipe', this);
    return this;
  }

  // try to find the right one.
  var i = indexOf(state.pipes, dest);
  if (i === -1)
    return this;

  state.pipes.splice(i, 1);
  state.pipesCount -= 1;
  if (state.pipesCount === 1)
    state.pipes = state.pipes[0];

  dest.emit('unpipe', this);

  return this;
};

// set up data events if they are asked for
// Ensure readable listeners eventually get something
Readable.prototype.on = function(ev, fn) {
  var res = Stream.prototype.on.call(this, ev, fn);

  if (ev === 'data' && !this._readableState.flowing)
    emitDataEvents(this);

  if (ev === 'readable' && this.readable) {
    var state = this._readableState;
    if (!state.readableListening) {
      state.readableListening = true;
      state.emittedReadable = false;
      state.needReadable = true;
      if (!state.reading) {
        this.read(0);
      } else if (state.length) {
        emitReadable(this, state);
      }
    }
  }

  return res;
};
Readable.prototype.addListener = Readable.prototype.on;

// pause() and resume() are remnants of the legacy readable stream API
// If the user uses them, then switch into old mode.
Readable.prototype.resume = function() {
  emitDataEvents(this);
  this.read(0);
  this.emit('resume');
};

Readable.prototype.pause = function() {
  emitDataEvents(this, true);
  this.emit('pause');
};

function emitDataEvents(stream, startPaused) {
  var state = stream._readableState;

  if (state.flowing) {
    // https://github.com/isaacs/readable-stream/issues/16
    throw new Error('Cannot switch to old mode now.');
  }

  var paused = startPaused || false;
  var readable = false;

  // convert to an old-style stream.
  stream.readable = true;
  stream.pipe = Stream.prototype.pipe;
  stream.on = stream.addListener = Stream.prototype.on;

  stream.on('readable', function() {
    readable = true;

    var c;
    while (!paused && (null !== (c = stream.read())))
      stream.emit('data', c);

    if (c === null) {
      readable = false;
      stream._readableState.needReadable = true;
    }
  });

  stream.pause = function() {
    paused = true;
    this.emit('pause');
  };

  stream.resume = function() {
    paused = false;
    if (readable)
      setImmediate(function() {
        stream.emit('readable');
      });
    else
      this.read(0);
    this.emit('resume');
  };

  // now make it start, just in case it hadn't already.
  stream.emit('readable');
}

// wrap an old-style stream as the async data source.
// This is *not* part of the readable stream interface.
// It is an ugly unfortunate mess of history.
Readable.prototype.wrap = function(stream) {
  var state = this._readableState;
  var paused = false;

  var self = this;
  stream.on('end', function() {
    if (state.decoder && !state.ended) {
      var chunk = state.decoder.end();
      if (chunk && chunk.length)
        self.push(chunk);
    }

    self.push(null);
  });

  stream.on('data', function(chunk) {
    if (state.decoder)
      chunk = state.decoder.write(chunk);
    if (!chunk || !state.objectMode && !chunk.length)
      return;

    var ret = self.push(chunk);
    if (!ret) {
      paused = true;
      stream.pause();
    }
  });

  // proxy all the other methods.
  // important when wrapping filters and duplexes.
  for (var i in stream) {
    if (typeof stream[i] === 'function' &&
        typeof this[i] === 'undefined') {
      this[i] = function(method) { return function() {
        return stream[method].apply(stream, arguments);
      }}(i);
    }
  }

  // proxy certain important events.
  var events = ['error', 'close', 'destroy', 'pause', 'resume'];
  forEach(events, function(ev) {
    stream.on(ev, function (x) {
      return self.emit.apply(self, ev, x);
    });
  });

  // when we try to consume some more bytes, simply unpause the
  // underlying stream.
  self._read = function(n) {
    if (paused) {
      paused = false;
      stream.resume();
    }
  };

  return self;
};



// exposed for testing purposes only.
Readable._fromList = fromList;

// Pluck off n bytes from an array of buffers.
// Length is the combined lengths of all the buffers in the list.
function fromList(n, state) {
  var list = state.buffer;
  var length = state.length;
  var stringMode = !!state.decoder;
  var objectMode = !!state.objectMode;
  var ret;

  // nothing in the list, definitely empty.
  if (list.length === 0)
    return null;

  if (length === 0)
    ret = null;
  else if (objectMode)
    ret = list.shift();
  else if (!n || n >= length) {
    // read it all, truncate the array.
    if (stringMode)
      ret = list.join('');
    else
      ret = Buffer.concat(list, length);
    list.length = 0;
  } else {
    // read just some of it.
    if (n < list[0].length) {
      // just take a part of the first list item.
      // slice is the same for buffers and strings.
      var buf = list[0];
      ret = buf.slice(0, n);
      list[0] = buf.slice(n);
    } else if (n === list[0].length) {
      // first list is a perfect match
      ret = list.shift();
    } else {
      // complex case.
      // we have enough to cover it, but it spans past the first buffer.
      if (stringMode)
        ret = '';
      else
        ret = new Buffer(n);

      var c = 0;
      for (var i = 0, l = list.length; i < l && c < n; i++) {
        var buf = list[0];
        var cpy = Math.min(n - c, buf.length);

        if (stringMode)
          ret += buf.slice(0, cpy);
        else
          buf.copy(ret, c, 0, cpy);

        if (cpy < buf.length)
          list[0] = buf.slice(cpy);
        else
          list.shift();

        c += cpy;
      }
    }
  }

  return ret;
}

function endReadable(stream) {
  var state = stream._readableState;

  // If we get here before consuming all the bytes, then that is a
  // bug in node.  Should never happen.
  if (state.length > 0)
    throw new Error('endReadable called on non-empty stream');

  if (!state.endEmitted && state.calledRead) {
    state.ended = true;
    setImmediate(function() {
      // Check that we didn't get one last unshift.
      if (!state.endEmitted && state.length === 0) {
        state.endEmitted = true;
        stream.readable = false;
        stream.emit('end');
      }
    });
  }
}

function forEach (xs, f) {
  for (var i = 0, l = xs.length; i < l; i++) {
    f(xs[i], i);
  }
}

function indexOf (xs, x) {
  for (var i = 0, l = xs.length; i < l; i++) {
    if (xs[i] === x) return i;
  }
  return -1;
}

}).call(this,require("/home/noah/Projects/shimi_ima/node_modules/browserify/node_modules/insert-module-globals/node_modules/process/browser.js"))
},{"./index.js":14,"/home/noah/Projects/shimi_ima/node_modules/browserify/node_modules/insert-module-globals/node_modules/process/browser.js":12,"buffer":1,"events":10,"inherits":11,"process/browser.js":15,"string_decoder":20}],18:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

// a transform stream is a readable/writable stream where you do
// something with the data.  Sometimes it's called a "filter",
// but that's not a great name for it, since that implies a thing where
// some bits pass through, and others are simply ignored.  (That would
// be a valid example of a transform, of course.)
//
// While the output is causally related to the input, it's not a
// necessarily symmetric or synchronous transformation.  For example,
// a zlib stream might take multiple plain-text writes(), and then
// emit a single compressed chunk some time in the future.
//
// Here's how this works:
//
// The Transform stream has all the aspects of the readable and writable
// stream classes.  When you write(chunk), that calls _write(chunk,cb)
// internally, and returns false if there's a lot of pending writes
// buffered up.  When you call read(), that calls _read(n) until
// there's enough pending readable data buffered up.
//
// In a transform stream, the written data is placed in a buffer.  When
// _read(n) is called, it transforms the queued up data, calling the
// buffered _write cb's as it consumes chunks.  If consuming a single
// written chunk would result in multiple output chunks, then the first
// outputted bit calls the readcb, and subsequent chunks just go into
// the read buffer, and will cause it to emit 'readable' if necessary.
//
// This way, back-pressure is actually determined by the reading side,
// since _read has to be called to start processing a new chunk.  However,
// a pathological inflate type of transform can cause excessive buffering
// here.  For example, imagine a stream where every byte of input is
// interpreted as an integer from 0-255, and then results in that many
// bytes of output.  Writing the 4 bytes {ff,ff,ff,ff} would result in
// 1kb of data being output.  In this case, you could write a very small
// amount of input, and end up with a very large amount of output.  In
// such a pathological inflating mechanism, there'd be no way to tell
// the system to stop doing the transform.  A single 4MB write could
// cause the system to run out of memory.
//
// However, even in such a pathological case, only a single written chunk
// would be consumed, and then the rest would wait (un-transformed) until
// the results of the previous transformed chunk were consumed.

module.exports = Transform;

var Duplex = require('./duplex.js');
var inherits = require('inherits');
inherits(Transform, Duplex);


function TransformState(options, stream) {
  this.afterTransform = function(er, data) {
    return afterTransform(stream, er, data);
  };

  this.needTransform = false;
  this.transforming = false;
  this.writecb = null;
  this.writechunk = null;
}

function afterTransform(stream, er, data) {
  var ts = stream._transformState;
  ts.transforming = false;

  var cb = ts.writecb;

  if (!cb)
    return stream.emit('error', new Error('no writecb in Transform class'));

  ts.writechunk = null;
  ts.writecb = null;

  if (data !== null && data !== undefined)
    stream.push(data);

  if (cb)
    cb(er);

  var rs = stream._readableState;
  rs.reading = false;
  if (rs.needReadable || rs.length < rs.highWaterMark) {
    stream._read(rs.highWaterMark);
  }
}


function Transform(options) {
  if (!(this instanceof Transform))
    return new Transform(options);

  Duplex.call(this, options);

  var ts = this._transformState = new TransformState(options, this);

  // when the writable side finishes, then flush out anything remaining.
  var stream = this;

  // start out asking for a readable event once data is transformed.
  this._readableState.needReadable = true;

  // we have implemented the _read method, and done the other things
  // that Readable wants before the first _read call, so unset the
  // sync guard flag.
  this._readableState.sync = false;

  this.once('finish', function() {
    if ('function' === typeof this._flush)
      this._flush(function(er) {
        done(stream, er);
      });
    else
      done(stream);
  });
}

Transform.prototype.push = function(chunk, encoding) {
  this._transformState.needTransform = false;
  return Duplex.prototype.push.call(this, chunk, encoding);
};

// This is the part where you do stuff!
// override this function in implementation classes.
// 'chunk' is an input chunk.
//
// Call `push(newChunk)` to pass along transformed output
// to the readable side.  You may call 'push' zero or more times.
//
// Call `cb(err)` when you are done with this chunk.  If you pass
// an error, then that'll put the hurt on the whole operation.  If you
// never call cb(), then you'll never get another chunk.
Transform.prototype._transform = function(chunk, encoding, cb) {
  throw new Error('not implemented');
};

Transform.prototype._write = function(chunk, encoding, cb) {
  var ts = this._transformState;
  ts.writecb = cb;
  ts.writechunk = chunk;
  ts.writeencoding = encoding;
  if (!ts.transforming) {
    var rs = this._readableState;
    if (ts.needTransform ||
        rs.needReadable ||
        rs.length < rs.highWaterMark)
      this._read(rs.highWaterMark);
  }
};

// Doesn't matter what the args are here.
// _transform does all the work.
// That we got here means that the readable side wants more data.
Transform.prototype._read = function(n) {
  var ts = this._transformState;

  if (ts.writechunk && ts.writecb && !ts.transforming) {
    ts.transforming = true;
    this._transform(ts.writechunk, ts.writeencoding, ts.afterTransform);
  } else {
    // mark that we need a transform, so that any data that comes in
    // will get processed, now that we've asked for it.
    ts.needTransform = true;
  }
};


function done(stream, er) {
  if (er)
    return stream.emit('error', er);

  // if there's nothing in the write buffer, then that means
  // that nothing more will ever be provided
  var ws = stream._writableState;
  var rs = stream._readableState;
  var ts = stream._transformState;

  if (ws.length)
    throw new Error('calling transform done when ws.length != 0');

  if (ts.transforming)
    throw new Error('calling transform done when still transforming');

  return stream.push(null);
}

},{"./duplex.js":13,"inherits":11}],19:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

// A bit simpler than readable streams.
// Implement an async ._write(chunk, cb), and it'll handle all
// the drain event emission and buffering.

module.exports = Writable;
Writable.WritableState = WritableState;

var isUint8Array = typeof Uint8Array !== 'undefined'
  ? function (x) { return x instanceof Uint8Array }
  : function (x) {
    return x && x.constructor && x.constructor.name === 'Uint8Array'
  }
;
var isArrayBuffer = typeof ArrayBuffer !== 'undefined'
  ? function (x) { return x instanceof ArrayBuffer }
  : function (x) {
    return x && x.constructor && x.constructor.name === 'ArrayBuffer'
  }
;

var inherits = require('inherits');
var Stream = require('./index.js');
var setImmediate = require('process/browser.js').nextTick;
var Buffer = require('buffer').Buffer;

inherits(Writable, Stream);

function WriteReq(chunk, encoding, cb) {
  this.chunk = chunk;
  this.encoding = encoding;
  this.callback = cb;
}

function WritableState(options, stream) {
  options = options || {};

  // the point at which write() starts returning false
  // Note: 0 is a valid value, means that we always return false if
  // the entire buffer is not flushed immediately on write()
  var hwm = options.highWaterMark;
  this.highWaterMark = (hwm || hwm === 0) ? hwm : 16 * 1024;

  // object stream flag to indicate whether or not this stream
  // contains buffers or objects.
  this.objectMode = !!options.objectMode;

  // cast to ints.
  this.highWaterMark = ~~this.highWaterMark;

  this.needDrain = false;
  // at the start of calling end()
  this.ending = false;
  // when end() has been called, and returned
  this.ended = false;
  // when 'finish' is emitted
  this.finished = false;

  // should we decode strings into buffers before passing to _write?
  // this is here so that some node-core streams can optimize string
  // handling at a lower level.
  var noDecode = options.decodeStrings === false;
  this.decodeStrings = !noDecode;

  // Crypto is kind of old and crusty.  Historically, its default string
  // encoding is 'binary' so we have to make this configurable.
  // Everything else in the universe uses 'utf8', though.
  this.defaultEncoding = options.defaultEncoding || 'utf8';

  // not an actual buffer we keep track of, but a measurement
  // of how much we're waiting to get pushed to some underlying
  // socket or file.
  this.length = 0;

  // a flag to see when we're in the middle of a write.
  this.writing = false;

  // a flag to be able to tell if the onwrite cb is called immediately,
  // or on a later tick.  We set this to true at first, becuase any
  // actions that shouldn't happen until "later" should generally also
  // not happen before the first write call.
  this.sync = true;

  // a flag to know if we're processing previously buffered items, which
  // may call the _write() callback in the same tick, so that we don't
  // end up in an overlapped onwrite situation.
  this.bufferProcessing = false;

  // the callback that's passed to _write(chunk,cb)
  this.onwrite = function(er) {
    onwrite(stream, er);
  };

  // the callback that the user supplies to write(chunk,encoding,cb)
  this.writecb = null;

  // the amount that is being written when _write is called.
  this.writelen = 0;

  this.buffer = [];
}

function Writable(options) {
  // Writable ctor is applied to Duplexes, though they're not
  // instanceof Writable, they're instanceof Readable.
  if (!(this instanceof Writable) && !(this instanceof Stream.Duplex))
    return new Writable(options);

  this._writableState = new WritableState(options, this);

  // legacy.
  this.writable = true;

  Stream.call(this);
}

// Otherwise people can pipe Writable streams, which is just wrong.
Writable.prototype.pipe = function() {
  this.emit('error', new Error('Cannot pipe. Not readable.'));
};


function writeAfterEnd(stream, state, cb) {
  var er = new Error('write after end');
  // TODO: defer error events consistently everywhere, not just the cb
  stream.emit('error', er);
  setImmediate(function() {
    cb(er);
  });
}

// If we get something that is not a buffer, string, null, or undefined,
// and we're not in objectMode, then that's an error.
// Otherwise stream chunks are all considered to be of length=1, and the
// watermarks determine how many objects to keep in the buffer, rather than
// how many bytes or characters.
function validChunk(stream, state, chunk, cb) {
  var valid = true;
  if (!Buffer.isBuffer(chunk) &&
      'string' !== typeof chunk &&
      chunk !== null &&
      chunk !== undefined &&
      !state.objectMode) {
    var er = new TypeError('Invalid non-string/buffer chunk');
    stream.emit('error', er);
    setImmediate(function() {
      cb(er);
    });
    valid = false;
  }
  return valid;
}

Writable.prototype.write = function(chunk, encoding, cb) {
  var state = this._writableState;
  var ret = false;

  if (typeof encoding === 'function') {
    cb = encoding;
    encoding = null;
  }

  if (!Buffer.isBuffer(chunk) && isUint8Array(chunk))
    chunk = new Buffer(chunk);
  if (isArrayBuffer(chunk) && typeof Uint8Array !== 'undefined')
    chunk = new Buffer(new Uint8Array(chunk));
  
  if (Buffer.isBuffer(chunk))
    encoding = 'buffer';
  else if (!encoding)
    encoding = state.defaultEncoding;

  if (typeof cb !== 'function')
    cb = function() {};

  if (state.ended)
    writeAfterEnd(this, state, cb);
  else if (validChunk(this, state, chunk, cb))
    ret = writeOrBuffer(this, state, chunk, encoding, cb);

  return ret;
};

function decodeChunk(state, chunk, encoding) {
  if (!state.objectMode &&
      state.decodeStrings !== false &&
      typeof chunk === 'string') {
    chunk = new Buffer(chunk, encoding);
  }
  return chunk;
}

// if we're already writing something, then just put this
// in the queue, and wait our turn.  Otherwise, call _write
// If we return false, then we need a drain event, so set that flag.
function writeOrBuffer(stream, state, chunk, encoding, cb) {
  chunk = decodeChunk(state, chunk, encoding);
  var len = state.objectMode ? 1 : chunk.length;

  state.length += len;

  var ret = state.length < state.highWaterMark;
  state.needDrain = !ret;

  if (state.writing)
    state.buffer.push(new WriteReq(chunk, encoding, cb));
  else
    doWrite(stream, state, len, chunk, encoding, cb);

  return ret;
}

function doWrite(stream, state, len, chunk, encoding, cb) {
  state.writelen = len;
  state.writecb = cb;
  state.writing = true;
  state.sync = true;
  stream._write(chunk, encoding, state.onwrite);
  state.sync = false;
}

function onwriteError(stream, state, sync, er, cb) {
  if (sync)
    setImmediate(function() {
      cb(er);
    });
  else
    cb(er);

  stream.emit('error', er);
}

function onwriteStateUpdate(state) {
  state.writing = false;
  state.writecb = null;
  state.length -= state.writelen;
  state.writelen = 0;
}

function onwrite(stream, er) {
  var state = stream._writableState;
  var sync = state.sync;
  var cb = state.writecb;

  onwriteStateUpdate(state);

  if (er)
    onwriteError(stream, state, sync, er, cb);
  else {
    // Check if we're actually ready to finish, but don't emit yet
    var finished = needFinish(stream, state);

    if (!finished && !state.bufferProcessing && state.buffer.length)
      clearBuffer(stream, state);

    if (sync) {
      setImmediate(function() {
        afterWrite(stream, state, finished, cb);
      });
    } else {
      afterWrite(stream, state, finished, cb);
    }
  }
}

function afterWrite(stream, state, finished, cb) {
  if (!finished)
    onwriteDrain(stream, state);
  cb();
  if (finished)
    finishMaybe(stream, state);
}

// Must force callback to be called on nextTick, so that we don't
// emit 'drain' before the write() consumer gets the 'false' return
// value, and has a chance to attach a 'drain' listener.
function onwriteDrain(stream, state) {
  if (state.length === 0 && state.needDrain) {
    state.needDrain = false;
    stream.emit('drain');
  }
}


// if there's something in the buffer waiting, then process it
function clearBuffer(stream, state) {
  state.bufferProcessing = true;

  for (var c = 0; c < state.buffer.length; c++) {
    var entry = state.buffer[c];
    var chunk = entry.chunk;
    var encoding = entry.encoding;
    var cb = entry.callback;
    var len = state.objectMode ? 1 : chunk.length;

    doWrite(stream, state, len, chunk, encoding, cb);

    // if we didn't call the onwrite immediately, then
    // it means that we need to wait until it does.
    // also, that means that the chunk and cb are currently
    // being processed, so move the buffer counter past them.
    if (state.writing) {
      c++;
      break;
    }
  }

  state.bufferProcessing = false;
  if (c < state.buffer.length)
    state.buffer = state.buffer.slice(c);
  else
    state.buffer.length = 0;
}

Writable.prototype._write = function(chunk, encoding, cb) {
  cb(new Error('not implemented'));
};

Writable.prototype.end = function(chunk, encoding, cb) {
  var state = this._writableState;

  if (typeof chunk === 'function') {
    cb = chunk;
    chunk = null;
    encoding = null;
  } else if (typeof encoding === 'function') {
    cb = encoding;
    encoding = null;
  }

  if (typeof chunk !== 'undefined' && chunk !== null)
    this.write(chunk, encoding);

  // ignore unnecessary end() calls.
  if (!state.ending && !state.finished)
    endWritable(this, state, cb);
};


function needFinish(stream, state) {
  return (state.ending &&
          state.length === 0 &&
          !state.finished &&
          !state.writing);
}

function finishMaybe(stream, state) {
  var need = needFinish(stream, state);
  if (need) {
    state.finished = true;
    stream.emit('finish');
  }
  return need;
}

function endWritable(stream, state, cb) {
  state.ending = true;
  finishMaybe(stream, state);
  if (cb) {
    if (state.finished)
      setImmediate(cb);
    else
      stream.once('finish', cb);
  }
  state.ended = true;
}

},{"./index.js":14,"buffer":1,"inherits":11,"process/browser.js":15}],20:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

var Buffer = require('buffer').Buffer;

function assertEncoding(encoding) {
  if (encoding && !Buffer.isEncoding(encoding)) {
    throw new Error('Unknown encoding: ' + encoding);
  }
}

var StringDecoder = exports.StringDecoder = function(encoding) {
  this.encoding = (encoding || 'utf8').toLowerCase().replace(/[-_]/, '');
  assertEncoding(encoding);
  switch (this.encoding) {
    case 'utf8':
      // CESU-8 represents each of Surrogate Pair by 3-bytes
      this.surrogateSize = 3;
      break;
    case 'ucs2':
    case 'utf16le':
      // UTF-16 represents each of Surrogate Pair by 2-bytes
      this.surrogateSize = 2;
      this.detectIncompleteChar = utf16DetectIncompleteChar;
      break;
    case 'base64':
      // Base-64 stores 3 bytes in 4 chars, and pads the remainder.
      this.surrogateSize = 3;
      this.detectIncompleteChar = base64DetectIncompleteChar;
      break;
    default:
      this.write = passThroughWrite;
      return;
  }

  this.charBuffer = new Buffer(6);
  this.charReceived = 0;
  this.charLength = 0;
};


StringDecoder.prototype.write = function(buffer) {
  var charStr = '';
  var offset = 0;

  // if our last write ended with an incomplete multibyte character
  while (this.charLength) {
    // determine how many remaining bytes this buffer has to offer for this char
    var i = (buffer.length >= this.charLength - this.charReceived) ?
                this.charLength - this.charReceived :
                buffer.length;

    // add the new bytes to the char buffer
    buffer.copy(this.charBuffer, this.charReceived, offset, i);
    this.charReceived += (i - offset);
    offset = i;

    if (this.charReceived < this.charLength) {
      // still not enough chars in this buffer? wait for more ...
      return '';
    }

    // get the character that was split
    charStr = this.charBuffer.slice(0, this.charLength).toString(this.encoding);

    // lead surrogate (D800-DBFF) is also the incomplete character
    var charCode = charStr.charCodeAt(charStr.length - 1);
    if (charCode >= 0xD800 && charCode <= 0xDBFF) {
      this.charLength += this.surrogateSize;
      charStr = '';
      continue;
    }
    this.charReceived = this.charLength = 0;

    // if there are no more bytes in this buffer, just emit our char
    if (i == buffer.length) return charStr;

    // otherwise cut off the characters end from the beginning of this buffer
    buffer = buffer.slice(i, buffer.length);
    break;
  }

  var lenIncomplete = this.detectIncompleteChar(buffer);

  var end = buffer.length;
  if (this.charLength) {
    // buffer the incomplete character bytes we got
    buffer.copy(this.charBuffer, 0, buffer.length - lenIncomplete, end);
    this.charReceived = lenIncomplete;
    end -= lenIncomplete;
  }

  charStr += buffer.toString(this.encoding, 0, end);

  var end = charStr.length - 1;
  var charCode = charStr.charCodeAt(end);
  // lead surrogate (D800-DBFF) is also the incomplete character
  if (charCode >= 0xD800 && charCode <= 0xDBFF) {
    var size = this.surrogateSize;
    this.charLength += size;
    this.charReceived += size;
    this.charBuffer.copy(this.charBuffer, size, 0, size);
    this.charBuffer.write(charStr.charAt(charStr.length - 1), this.encoding);
    return charStr.substring(0, end);
  }

  // or just emit the charStr
  return charStr;
};

StringDecoder.prototype.detectIncompleteChar = function(buffer) {
  // determine how many bytes we have to check at the end of this buffer
  var i = (buffer.length >= 3) ? 3 : buffer.length;

  // Figure out if one of the last i bytes of our buffer announces an
  // incomplete char.
  for (; i > 0; i--) {
    var c = buffer[buffer.length - i];

    // See http://en.wikipedia.org/wiki/UTF-8#Description

    // 110XXXXX
    if (i == 1 && c >> 5 == 0x06) {
      this.charLength = 2;
      break;
    }

    // 1110XXXX
    if (i <= 2 && c >> 4 == 0x0E) {
      this.charLength = 3;
      break;
    }

    // 11110XXX
    if (i <= 3 && c >> 3 == 0x1E) {
      this.charLength = 4;
      break;
    }
  }

  return i;
};

StringDecoder.prototype.end = function(buffer) {
  var res = '';
  if (buffer && buffer.length)
    res = this.write(buffer);

  if (this.charReceived) {
    var cr = this.charReceived;
    var buf = this.charBuffer;
    var enc = this.encoding;
    res += buf.slice(0, cr).toString(enc);
  }

  return res;
};

function passThroughWrite(buffer) {
  return buffer.toString(this.encoding);
}

function utf16DetectIncompleteChar(buffer) {
  var incomplete = this.charReceived = buffer.length % 2;
  this.charLength = incomplete ? 2 : 0;
  return incomplete;
}

function base64DetectIncompleteChar(buffer) {
  var incomplete = this.charReceived = buffer.length % 3;
  this.charLength = incomplete ? 3 : 0;
  return incomplete;
}

},{"buffer":1}],21:[function(require,module,exports){
module.exports = function isBuffer(arg) {
  return arg && typeof arg === 'object'
    && typeof arg.copy === 'function'
    && typeof arg.fill === 'function'
    && typeof arg.readUInt8 === 'function';
}
},{}],22:[function(require,module,exports){
(function (process,global){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

var formatRegExp = /%[sdj%]/g;
exports.format = function(f) {
  if (!isString(f)) {
    var objects = [];
    for (var i = 0; i < arguments.length; i++) {
      objects.push(inspect(arguments[i]));
    }
    return objects.join(' ');
  }

  var i = 1;
  var args = arguments;
  var len = args.length;
  var str = String(f).replace(formatRegExp, function(x) {
    if (x === '%%') return '%';
    if (i >= len) return x;
    switch (x) {
      case '%s': return String(args[i++]);
      case '%d': return Number(args[i++]);
      case '%j':
        try {
          return JSON.stringify(args[i++]);
        } catch (_) {
          return '[Circular]';
        }
      default:
        return x;
    }
  });
  for (var x = args[i]; i < len; x = args[++i]) {
    if (isNull(x) || !isObject(x)) {
      str += ' ' + x;
    } else {
      str += ' ' + inspect(x);
    }
  }
  return str;
};


// Mark that a method should not be used.
// Returns a modified function which warns once by default.
// If --no-deprecation is set, then it is a no-op.
exports.deprecate = function(fn, msg) {
  // Allow for deprecating things in the process of starting up.
  if (isUndefined(global.process)) {
    return function() {
      return exports.deprecate(fn, msg).apply(this, arguments);
    };
  }

  if (process.noDeprecation === true) {
    return fn;
  }

  var warned = false;
  function deprecated() {
    if (!warned) {
      if (process.throwDeprecation) {
        throw new Error(msg);
      } else if (process.traceDeprecation) {
        console.trace(msg);
      } else {
        console.error(msg);
      }
      warned = true;
    }
    return fn.apply(this, arguments);
  }

  return deprecated;
};


var debugs = {};
var debugEnviron;
exports.debuglog = function(set) {
  if (isUndefined(debugEnviron))
    debugEnviron = process.env.NODE_DEBUG || '';
  set = set.toUpperCase();
  if (!debugs[set]) {
    if (new RegExp('\\b' + set + '\\b', 'i').test(debugEnviron)) {
      var pid = process.pid;
      debugs[set] = function() {
        var msg = exports.format.apply(exports, arguments);
        console.error('%s %d: %s', set, pid, msg);
      };
    } else {
      debugs[set] = function() {};
    }
  }
  return debugs[set];
};


/**
 * Echos the value of a value. Trys to print the value out
 * in the best way possible given the different types.
 *
 * @param {Object} obj The object to print out.
 * @param {Object} opts Optional options object that alters the output.
 */
/* legacy: obj, showHidden, depth, colors*/
function inspect(obj, opts) {
  // default options
  var ctx = {
    seen: [],
    stylize: stylizeNoColor
  };
  // legacy...
  if (arguments.length >= 3) ctx.depth = arguments[2];
  if (arguments.length >= 4) ctx.colors = arguments[3];
  if (isBoolean(opts)) {
    // legacy...
    ctx.showHidden = opts;
  } else if (opts) {
    // got an "options" object
    exports._extend(ctx, opts);
  }
  // set default options
  if (isUndefined(ctx.showHidden)) ctx.showHidden = false;
  if (isUndefined(ctx.depth)) ctx.depth = 2;
  if (isUndefined(ctx.colors)) ctx.colors = false;
  if (isUndefined(ctx.customInspect)) ctx.customInspect = true;
  if (ctx.colors) ctx.stylize = stylizeWithColor;
  return formatValue(ctx, obj, ctx.depth);
}
exports.inspect = inspect;


// http://en.wikipedia.org/wiki/ANSI_escape_code#graphics
inspect.colors = {
  'bold' : [1, 22],
  'italic' : [3, 23],
  'underline' : [4, 24],
  'inverse' : [7, 27],
  'white' : [37, 39],
  'grey' : [90, 39],
  'black' : [30, 39],
  'blue' : [34, 39],
  'cyan' : [36, 39],
  'green' : [32, 39],
  'magenta' : [35, 39],
  'red' : [31, 39],
  'yellow' : [33, 39]
};

// Don't use 'blue' not visible on cmd.exe
inspect.styles = {
  'special': 'cyan',
  'number': 'yellow',
  'boolean': 'yellow',
  'undefined': 'grey',
  'null': 'bold',
  'string': 'green',
  'date': 'magenta',
  // "name": intentionally not styling
  'regexp': 'red'
};


function stylizeWithColor(str, styleType) {
  var style = inspect.styles[styleType];

  if (style) {
    return '\u001b[' + inspect.colors[style][0] + 'm' + str +
           '\u001b[' + inspect.colors[style][1] + 'm';
  } else {
    return str;
  }
}


function stylizeNoColor(str, styleType) {
  return str;
}


function arrayToHash(array) {
  var hash = {};

  array.forEach(function(val, idx) {
    hash[val] = true;
  });

  return hash;
}


function formatValue(ctx, value, recurseTimes) {
  // Provide a hook for user-specified inspect functions.
  // Check that value is an object with an inspect function on it
  if (ctx.customInspect &&
      value &&
      isFunction(value.inspect) &&
      // Filter out the util module, it's inspect function is special
      value.inspect !== exports.inspect &&
      // Also filter out any prototype objects using the circular check.
      !(value.constructor && value.constructor.prototype === value)) {
    var ret = value.inspect(recurseTimes, ctx);
    if (!isString(ret)) {
      ret = formatValue(ctx, ret, recurseTimes);
    }
    return ret;
  }

  // Primitive types cannot have properties
  var primitive = formatPrimitive(ctx, value);
  if (primitive) {
    return primitive;
  }

  // Look up the keys of the object.
  var keys = Object.keys(value);
  var visibleKeys = arrayToHash(keys);

  if (ctx.showHidden) {
    keys = Object.getOwnPropertyNames(value);
  }

  // IE doesn't make error fields non-enumerable
  // http://msdn.microsoft.com/en-us/library/ie/dww52sbt(v=vs.94).aspx
  if (isError(value)
      && (keys.indexOf('message') >= 0 || keys.indexOf('description') >= 0)) {
    return formatError(value);
  }

  // Some type of object without properties can be shortcutted.
  if (keys.length === 0) {
    if (isFunction(value)) {
      var name = value.name ? ': ' + value.name : '';
      return ctx.stylize('[Function' + name + ']', 'special');
    }
    if (isRegExp(value)) {
      return ctx.stylize(RegExp.prototype.toString.call(value), 'regexp');
    }
    if (isDate(value)) {
      return ctx.stylize(Date.prototype.toString.call(value), 'date');
    }
    if (isError(value)) {
      return formatError(value);
    }
  }

  var base = '', array = false, braces = ['{', '}'];

  // Make Array say that they are Array
  if (isArray(value)) {
    array = true;
    braces = ['[', ']'];
  }

  // Make functions say that they are functions
  if (isFunction(value)) {
    var n = value.name ? ': ' + value.name : '';
    base = ' [Function' + n + ']';
  }

  // Make RegExps say that they are RegExps
  if (isRegExp(value)) {
    base = ' ' + RegExp.prototype.toString.call(value);
  }

  // Make dates with properties first say the date
  if (isDate(value)) {
    base = ' ' + Date.prototype.toUTCString.call(value);
  }

  // Make error with message first say the error
  if (isError(value)) {
    base = ' ' + formatError(value);
  }

  if (keys.length === 0 && (!array || value.length == 0)) {
    return braces[0] + base + braces[1];
  }

  if (recurseTimes < 0) {
    if (isRegExp(value)) {
      return ctx.stylize(RegExp.prototype.toString.call(value), 'regexp');
    } else {
      return ctx.stylize('[Object]', 'special');
    }
  }

  ctx.seen.push(value);

  var output;
  if (array) {
    output = formatArray(ctx, value, recurseTimes, visibleKeys, keys);
  } else {
    output = keys.map(function(key) {
      return formatProperty(ctx, value, recurseTimes, visibleKeys, key, array);
    });
  }

  ctx.seen.pop();

  return reduceToSingleString(output, base, braces);
}


function formatPrimitive(ctx, value) {
  if (isUndefined(value))
    return ctx.stylize('undefined', 'undefined');
  if (isString(value)) {
    var simple = '\'' + JSON.stringify(value).replace(/^"|"$/g, '')
                                             .replace(/'/g, "\\'")
                                             .replace(/\\"/g, '"') + '\'';
    return ctx.stylize(simple, 'string');
  }
  if (isNumber(value))
    return ctx.stylize('' + value, 'number');
  if (isBoolean(value))
    return ctx.stylize('' + value, 'boolean');
  // For some reason typeof null is "object", so special case here.
  if (isNull(value))
    return ctx.stylize('null', 'null');
}


function formatError(value) {
  return '[' + Error.prototype.toString.call(value) + ']';
}


function formatArray(ctx, value, recurseTimes, visibleKeys, keys) {
  var output = [];
  for (var i = 0, l = value.length; i < l; ++i) {
    if (hasOwnProperty(value, String(i))) {
      output.push(formatProperty(ctx, value, recurseTimes, visibleKeys,
          String(i), true));
    } else {
      output.push('');
    }
  }
  keys.forEach(function(key) {
    if (!key.match(/^\d+$/)) {
      output.push(formatProperty(ctx, value, recurseTimes, visibleKeys,
          key, true));
    }
  });
  return output;
}


function formatProperty(ctx, value, recurseTimes, visibleKeys, key, array) {
  var name, str, desc;
  desc = Object.getOwnPropertyDescriptor(value, key) || { value: value[key] };
  if (desc.get) {
    if (desc.set) {
      str = ctx.stylize('[Getter/Setter]', 'special');
    } else {
      str = ctx.stylize('[Getter]', 'special');
    }
  } else {
    if (desc.set) {
      str = ctx.stylize('[Setter]', 'special');
    }
  }
  if (!hasOwnProperty(visibleKeys, key)) {
    name = '[' + key + ']';
  }
  if (!str) {
    if (ctx.seen.indexOf(desc.value) < 0) {
      if (isNull(recurseTimes)) {
        str = formatValue(ctx, desc.value, null);
      } else {
        str = formatValue(ctx, desc.value, recurseTimes - 1);
      }
      if (str.indexOf('\n') > -1) {
        if (array) {
          str = str.split('\n').map(function(line) {
            return '  ' + line;
          }).join('\n').substr(2);
        } else {
          str = '\n' + str.split('\n').map(function(line) {
            return '   ' + line;
          }).join('\n');
        }
      }
    } else {
      str = ctx.stylize('[Circular]', 'special');
    }
  }
  if (isUndefined(name)) {
    if (array && key.match(/^\d+$/)) {
      return str;
    }
    name = JSON.stringify('' + key);
    if (name.match(/^"([a-zA-Z_][a-zA-Z_0-9]*)"$/)) {
      name = name.substr(1, name.length - 2);
      name = ctx.stylize(name, 'name');
    } else {
      name = name.replace(/'/g, "\\'")
                 .replace(/\\"/g, '"')
                 .replace(/(^"|"$)/g, "'");
      name = ctx.stylize(name, 'string');
    }
  }

  return name + ': ' + str;
}


function reduceToSingleString(output, base, braces) {
  var numLinesEst = 0;
  var length = output.reduce(function(prev, cur) {
    numLinesEst++;
    if (cur.indexOf('\n') >= 0) numLinesEst++;
    return prev + cur.replace(/\u001b\[\d\d?m/g, '').length + 1;
  }, 0);

  if (length > 60) {
    return braces[0] +
           (base === '' ? '' : base + '\n ') +
           ' ' +
           output.join(',\n  ') +
           ' ' +
           braces[1];
  }

  return braces[0] + base + ' ' + output.join(', ') + ' ' + braces[1];
}


// NOTE: These type checking functions intentionally don't use `instanceof`
// because it is fragile and can be easily faked with `Object.create()`.
function isArray(ar) {
  return Array.isArray(ar);
}
exports.isArray = isArray;

function isBoolean(arg) {
  return typeof arg === 'boolean';
}
exports.isBoolean = isBoolean;

function isNull(arg) {
  return arg === null;
}
exports.isNull = isNull;

function isNullOrUndefined(arg) {
  return arg == null;
}
exports.isNullOrUndefined = isNullOrUndefined;

function isNumber(arg) {
  return typeof arg === 'number';
}
exports.isNumber = isNumber;

function isString(arg) {
  return typeof arg === 'string';
}
exports.isString = isString;

function isSymbol(arg) {
  return typeof arg === 'symbol';
}
exports.isSymbol = isSymbol;

function isUndefined(arg) {
  return arg === void 0;
}
exports.isUndefined = isUndefined;

function isRegExp(re) {
  return isObject(re) && objectToString(re) === '[object RegExp]';
}
exports.isRegExp = isRegExp;

function isObject(arg) {
  return typeof arg === 'object' && arg !== null;
}
exports.isObject = isObject;

function isDate(d) {
  return isObject(d) && objectToString(d) === '[object Date]';
}
exports.isDate = isDate;

function isError(e) {
  return isObject(e) &&
      (objectToString(e) === '[object Error]' || e instanceof Error);
}
exports.isError = isError;

function isFunction(arg) {
  return typeof arg === 'function';
}
exports.isFunction = isFunction;

function isPrimitive(arg) {
  return arg === null ||
         typeof arg === 'boolean' ||
         typeof arg === 'number' ||
         typeof arg === 'string' ||
         typeof arg === 'symbol' ||  // ES6 symbol
         typeof arg === 'undefined';
}
exports.isPrimitive = isPrimitive;

exports.isBuffer = require('./support/isBuffer');

function objectToString(o) {
  return Object.prototype.toString.call(o);
}


function pad(n) {
  return n < 10 ? '0' + n.toString(10) : n.toString(10);
}


var months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
              'Oct', 'Nov', 'Dec'];

// 26 Feb 16:19:34
function timestamp() {
  var d = new Date();
  var time = [pad(d.getHours()),
              pad(d.getMinutes()),
              pad(d.getSeconds())].join(':');
  return [d.getDate(), months[d.getMonth()], time].join(' ');
}


// log is just a thin wrapper to console.log that prepends a timestamp
exports.log = function() {
  console.log('%s - %s', timestamp(), exports.format.apply(exports, arguments));
};


/**
 * Inherit the prototype methods from one constructor into another.
 *
 * The Function.prototype.inherits from lang.js rewritten as a standalone
 * function (not on Function.prototype). NOTE: If this file is to be loaded
 * during bootstrapping this function needs to be rewritten using some native
 * functions as prototype setup using normal JavaScript does not work as
 * expected during bootstrapping (see mirror.js in r114903).
 *
 * @param {function} ctor Constructor function which needs to inherit the
 *     prototype.
 * @param {function} superCtor Constructor function to inherit prototype from.
 */
exports.inherits = require('inherits');

exports._extend = function(origin, add) {
  // Don't do anything if add isn't an object
  if (!add || !isObject(add)) return origin;

  var keys = Object.keys(add);
  var i = keys.length;
  while (i--) {
    origin[keys[i]] = add[keys[i]];
  }
  return origin;
};

function hasOwnProperty(obj, prop) {
  return Object.prototype.hasOwnProperty.call(obj, prop);
}

}).call(this,require("/home/noah/Projects/shimi_ima/node_modules/browserify/node_modules/insert-module-globals/node_modules/process/browser.js"),typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./support/isBuffer":21,"/home/noah/Projects/shimi_ima/node_modules/browserify/node_modules/insert-module-globals/node_modules/process/browser.js":12,"inherits":11}],23:[function(require,module,exports){
/*
 *  Copyright 2011 Twitter, Inc.
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

(function (Hogan) {
  // Setup regex  assignments
  // remove whitespace according to Mustache spec
  var rIsWhitespace = /\S/,
      rQuot = /\"/g,
      rNewline =  /\n/g,
      rCr = /\r/g,
      rSlash = /\\/g,
      tagTypes = {
        '#': 1, '^': 2, '/': 3,  '!': 4, '>': 5,
        '<': 6, '=': 7, '_v': 8, '{': 9, '&': 10
      };

  Hogan.scan = function scan(text, delimiters) {
    var len = text.length,
        IN_TEXT = 0,
        IN_TAG_TYPE = 1,
        IN_TAG = 2,
        state = IN_TEXT,
        tagType = null,
        tag = null,
        buf = '',
        tokens = [],
        seenTag = false,
        i = 0,
        lineStart = 0,
        otag = '{{',
        ctag = '}}';

    function addBuf() {
      if (buf.length > 0) {
        tokens.push(new String(buf));
        buf = '';
      }
    }

    function lineIsWhitespace() {
      var isAllWhitespace = true;
      for (var j = lineStart; j < tokens.length; j++) {
        isAllWhitespace =
          (tokens[j].tag && tagTypes[tokens[j].tag] < tagTypes['_v']) ||
          (!tokens[j].tag && tokens[j].match(rIsWhitespace) === null);
        if (!isAllWhitespace) {
          return false;
        }
      }

      return isAllWhitespace;
    }

    function filterLine(haveSeenTag, noNewLine) {
      addBuf();

      if (haveSeenTag && lineIsWhitespace()) {
        for (var j = lineStart, next; j < tokens.length; j++) {
          if (!tokens[j].tag) {
            if ((next = tokens[j+1]) && next.tag == '>') {
              // set indent to token value
              next.indent = tokens[j].toString()
            }
            tokens.splice(j, 1);
          }
        }
      } else if (!noNewLine) {
        tokens.push({tag:'\n'});
      }

      seenTag = false;
      lineStart = tokens.length;
    }

    function changeDelimiters(text, index) {
      var close = '=' + ctag,
          closeIndex = text.indexOf(close, index),
          delimiters = trim(
            text.substring(text.indexOf('=', index) + 1, closeIndex)
          ).split(' ');

      otag = delimiters[0];
      ctag = delimiters[1];

      return closeIndex + close.length - 1;
    }

    if (delimiters) {
      delimiters = delimiters.split(' ');
      otag = delimiters[0];
      ctag = delimiters[1];
    }

    for (i = 0; i < len; i++) {
      if (state == IN_TEXT) {
        if (tagChange(otag, text, i)) {
          --i;
          addBuf();
          state = IN_TAG_TYPE;
        } else {
          if (text.charAt(i) == '\n') {
            filterLine(seenTag);
          } else {
            buf += text.charAt(i);
          }
        }
      } else if (state == IN_TAG_TYPE) {
        i += otag.length - 1;
        tag = tagTypes[text.charAt(i + 1)];
        tagType = tag ? text.charAt(i + 1) : '_v';
        if (tagType == '=') {
          i = changeDelimiters(text, i);
          state = IN_TEXT;
        } else {
          if (tag) {
            i++;
          }
          state = IN_TAG;
        }
        seenTag = i;
      } else {
        if (tagChange(ctag, text, i)) {
          tokens.push({tag: tagType, n: trim(buf), otag: otag, ctag: ctag,
                       i: (tagType == '/') ? seenTag - ctag.length : i + otag.length});
          buf = '';
          i += ctag.length - 1;
          state = IN_TEXT;
          if (tagType == '{') {
            if (ctag == '}}') {
              i++;
            } else {
              cleanTripleStache(tokens[tokens.length - 1]);
            }
          }
        } else {
          buf += text.charAt(i);
        }
      }
    }

    filterLine(seenTag, true);

    return tokens;
  }

  function cleanTripleStache(token) {
    if (token.n.substr(token.n.length - 1) === '}') {
      token.n = token.n.substring(0, token.n.length - 1);
    }
  }

  function trim(s) {
    if (s.trim) {
      return s.trim();
    }

    return s.replace(/^\s*|\s*$/g, '');
  }

  function tagChange(tag, text, index) {
    if (text.charAt(index) != tag.charAt(0)) {
      return false;
    }

    for (var i = 1, l = tag.length; i < l; i++) {
      if (text.charAt(index + i) != tag.charAt(i)) {
        return false;
      }
    }

    return true;
  }

  function buildTree(tokens, kind, stack, customTags) {
    var instructions = [],
        opener = null,
        token = null;

    while (tokens.length > 0) {
      token = tokens.shift();
      if (token.tag == '#' || token.tag == '^' || isOpener(token, customTags)) {
        stack.push(token);
        token.nodes = buildTree(tokens, token.tag, stack, customTags);
        instructions.push(token);
      } else if (token.tag == '/') {
        if (stack.length === 0) {
          throw new Error('Closing tag without opener: /' + token.n);
        }
        opener = stack.pop();
        if (token.n != opener.n && !isCloser(token.n, opener.n, customTags)) {
          throw new Error('Nesting error: ' + opener.n + ' vs. ' + token.n);
        }
        opener.end = token.i;
        return instructions;
      } else {
        instructions.push(token);
      }
    }

    if (stack.length > 0) {
      throw new Error('missing closing tag: ' + stack.pop().n);
    }

    return instructions;
  }

  function isOpener(token, tags) {
    for (var i = 0, l = tags.length; i < l; i++) {
      if (tags[i].o == token.n) {
        token.tag = '#';
        return true;
      }
    }
  }

  function isCloser(close, open, tags) {
    for (var i = 0, l = tags.length; i < l; i++) {
      if (tags[i].c == close && tags[i].o == open) {
        return true;
      }
    }
  }

  Hogan.generate = function (tree, text, options) {
    var code = 'var _=this;_.b(i=i||"");' + walk(tree) + 'return _.fl();';
    if (options.asString) {
      return 'function(c,p,i){' + code + ';}';
    }

    return new Hogan.Template(new Function('c', 'p', 'i', code), text, Hogan, options);
  }

  function esc(s) {
    return s.replace(rSlash, '\\\\')
            .replace(rQuot, '\\\"')
            .replace(rNewline, '\\n')
            .replace(rCr, '\\r');
  }

  function chooseMethod(s) {
    return (~s.indexOf('.')) ? 'd' : 'f';
  }

  function walk(tree) {
    var code = '';
    for (var i = 0, l = tree.length; i < l; i++) {
      var tag = tree[i].tag;
      if (tag == '#') {
        code += section(tree[i].nodes, tree[i].n, chooseMethod(tree[i].n),
                        tree[i].i, tree[i].end, tree[i].otag + " " + tree[i].ctag);
      } else if (tag == '^') {
        code += invertedSection(tree[i].nodes, tree[i].n,
                                chooseMethod(tree[i].n));
      } else if (tag == '<' || tag == '>') {
        code += partial(tree[i]);
      } else if (tag == '{' || tag == '&') {
        code += tripleStache(tree[i].n, chooseMethod(tree[i].n));
      } else if (tag == '\n') {
        code += text('"\\n"' + (tree.length-1 == i ? '' : ' + i'));
      } else if (tag == '_v') {
        code += variable(tree[i].n, chooseMethod(tree[i].n));
      } else if (tag === undefined) {
        code += text('"' + esc(tree[i]) + '"');
      }
    }
    return code;
  }

  function section(nodes, id, method, start, end, tags) {
    return 'if(_.s(_.' + method + '("' + esc(id) + '",c,p,1),' +
           'c,p,0,' + start + ',' + end + ',"' + tags + '")){' +
           '_.rs(c,p,' +
           'function(c,p,_){' +
           walk(nodes) +
           '});c.pop();}';
  }

  function invertedSection(nodes, id, method) {
    return 'if(!_.s(_.' + method + '("' + esc(id) + '",c,p,1),c,p,1,0,0,"")){' +
           walk(nodes) +
           '};';
  }

  function partial(tok) {
    return '_.b(_.rp("' +  esc(tok.n) + '",c,p,"' + (tok.indent || '') + '"));';
  }

  function tripleStache(id, method) {
    return '_.b(_.t(_.' + method + '("' + esc(id) + '",c,p,0)));';
  }

  function variable(id, method) {
    return '_.b(_.v(_.' + method + '("' + esc(id) + '",c,p,0)));';
  }

  function text(id) {
    return '_.b(' + id + ');';
  }

  Hogan.parse = function(tokens, text, options) {
    options = options || {};
    return buildTree(tokens, '', [], options.sectionTags || []);
  },

  Hogan.cache = {};

  Hogan.compile = function(text, options) {
    // options
    //
    // asString: false (default)
    //
    // sectionTags: [{o: '_foo', c: 'foo'}]
    // An array of object with o and c fields that indicate names for custom
    // section tags. The example above allows parsing of {{_foo}}{{/foo}}.
    //
    // delimiters: A string that overrides the default delimiters.
    // Example: "<% %>"
    //
    options = options || {};

    var key = text + '||' + !!options.asString;

    var t = this.cache[key];

    if (t) {
      return t;
    }

    t = this.generate(this.parse(this.scan(text, options.delimiters), text, options), text, options);
    return this.cache[key] = t;
  };
})(typeof exports !== 'undefined' ? exports : Hogan);

},{}],24:[function(require,module,exports){
/*
 *  Copyright 2011 Twitter, Inc.
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

// This file is for use with Node.js. See dist/ for browser files.

var Hogan = require('./compiler');
Hogan.Template = require('./template').Template;
module.exports = Hogan; 
},{"./compiler":23,"./template":25}],25:[function(require,module,exports){
/*
 *  Copyright 2011 Twitter, Inc.
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

var Hogan = {};

(function (Hogan, useArrayBuffer) {
  Hogan.Template = function (renderFunc, text, compiler, options) {
    this.r = renderFunc || this.r;
    this.c = compiler;
    this.options = options;
    this.text = text || '';
    this.buf = (useArrayBuffer) ? [] : '';
  }

  Hogan.Template.prototype = {
    // render: replaced by generated code.
    r: function (context, partials, indent) { return ''; },

    // variable escaping
    v: hoganEscape,

    // triple stache
    t: coerceToString,

    render: function render(context, partials, indent) {
      return this.ri([context], partials || {}, indent);
    },

    // render internal -- a hook for overrides that catches partials too
    ri: function (context, partials, indent) {
      return this.r(context, partials, indent);
    },

    // tries to find a partial in the curent scope and render it
    rp: function(name, context, partials, indent) {
      var partial = partials[name];

      if (!partial) {
        return '';
      }

      if (this.c && typeof partial == 'string') {
        partial = this.c.compile(partial, this.options);
      }

      return partial.ri(context, partials, indent);
    },

    // render a section
    rs: function(context, partials, section) {
      var tail = context[context.length - 1];

      if (!isArray(tail)) {
        section(context, partials, this);
        return;
      }

      for (var i = 0; i < tail.length; i++) {
        context.push(tail[i]);
        section(context, partials, this);
        context.pop();
      }
    },

    // maybe start a section
    s: function(val, ctx, partials, inverted, start, end, tags) {
      var pass;

      if (isArray(val) && val.length === 0) {
        return false;
      }

      if (typeof val == 'function') {
        val = this.ls(val, ctx, partials, inverted, start, end, tags);
      }

      pass = (val === '') || !!val;

      if (!inverted && pass && ctx) {
        ctx.push((typeof val == 'object') ? val : ctx[ctx.length - 1]);
      }

      return pass;
    },

    // find values with dotted names
    d: function(key, ctx, partials, returnFound) {
      var names = key.split('.'),
          val = this.f(names[0], ctx, partials, returnFound),
          cx = null;

      if (key === '.' && isArray(ctx[ctx.length - 2])) {
        return ctx[ctx.length - 1];
      }

      for (var i = 1; i < names.length; i++) {
        if (val && typeof val == 'object' && names[i] in val) {
          cx = val;
          val = val[names[i]];
        } else {
          val = '';
        }
      }

      if (returnFound && !val) {
        return false;
      }

      if (!returnFound && typeof val == 'function') {
        ctx.push(cx);
        val = this.lv(val, ctx, partials);
        ctx.pop();
      }

      return val;
    },

    // find values with normal names
    f: function(key, ctx, partials, returnFound) {
      var val = false,
          v = null,
          found = false;

      for (var i = ctx.length - 1; i >= 0; i--) {
        v = ctx[i];
        if (v && typeof v == 'object' && key in v) {
          val = v[key];
          found = true;
          break;
        }
      }

      if (!found) {
        return (returnFound) ? false : "";
      }

      if (!returnFound && typeof val == 'function') {
        val = this.lv(val, ctx, partials);
      }

      return val;
    },

    // higher order templates
    ho: function(val, cx, partials, text, tags) {
      var compiler = this.c;
      var options = this.options;
      options.delimiters = tags;
      var text = val.call(cx, text);
      text = (text == null) ? String(text) : text.toString();
      this.b(compiler.compile(text, options).render(cx, partials));
      return false;
    },

    // template result buffering
    b: (useArrayBuffer) ? function(s) { this.buf.push(s); } :
                          function(s) { this.buf += s; },
    fl: (useArrayBuffer) ? function() { var r = this.buf.join(''); this.buf = []; return r; } :
                           function() { var r = this.buf; this.buf = ''; return r; },

    // lambda replace section
    ls: function(val, ctx, partials, inverted, start, end, tags) {
      var cx = ctx[ctx.length - 1],
          t = null;

      if (!inverted && this.c && val.length > 0) {
        return this.ho(val, cx, partials, this.text.substring(start, end), tags);
      }

      t = val.call(cx);

      if (typeof t == 'function') {
        if (inverted) {
          return true;
        } else if (this.c) {
          return this.ho(t, cx, partials, this.text.substring(start, end), tags);
        }
      }

      return t;
    },

    // lambda replace variable
    lv: function(val, ctx, partials) {
      var cx = ctx[ctx.length - 1];
      var result = val.call(cx);

      if (typeof result == 'function') {
        result = coerceToString(result.call(cx));
        if (this.c && ~result.indexOf("{\u007B")) {
          return this.c.compile(result, this.options).render(cx, partials);
        }
      }

      return coerceToString(result);
    }

  };

  var rAmp = /&/g,
      rLt = /</g,
      rGt = />/g,
      rApos =/\'/g,
      rQuot = /\"/g,
      hChars =/[&<>\"\']/;


  function coerceToString(val) {
    return String((val === null || val === undefined) ? '' : val);
  }

  function hoganEscape(str) {
    str = coerceToString(str);
    return hChars.test(str) ?
      str
        .replace(rAmp,'&amp;')
        .replace(rLt,'&lt;')
        .replace(rGt,'&gt;')
        .replace(rApos,'&#39;')
        .replace(rQuot, '&quot;') :
      str;
  }

  var isArray = Array.isArray || function(a) {
    return Object.prototype.toString.call(a) === '[object Array]';
  };

})(typeof exports !== 'undefined' ? exports : Hogan);


},{}],26:[function(require,module,exports){
module.exports = CollectingHandler;

function CollectingHandler(cbs){
	this._cbs = cbs || {};
	this.events = [];
}

var EVENTS = require("./").EVENTS;
Object.keys(EVENTS).forEach(function(name){
	if(EVENTS[name] === 0){
		name = "on" + name;
		CollectingHandler.prototype[name] = function(){
			this.events.push([name]);
			if(this._cbs[name]) this._cbs[name]();
		};
	} else if(EVENTS[name] === 1){
		name = "on" + name;
		CollectingHandler.prototype[name] = function(a){
			this.events.push([name, a]);
			if(this._cbs[name]) this._cbs[name](a);
		};
	} else if(EVENTS[name] === 2){
		name = "on" + name;
		CollectingHandler.prototype[name] = function(a, b){
			this.events.push([name, a, b]);
			if(this._cbs[name]) this._cbs[name](a, b);
		};
	} else {
		throw Error("wrong number of arguments");
	}
});

CollectingHandler.prototype.onreset = function(){
	this.events = [];
	if(this._cbs.onreset) this._cbs.onreset();
};

CollectingHandler.prototype.restart = function(){
	if(this._cbs.onreset) this._cbs.onreset();

	for(var i = 0, len = this.events.length; i < len; i++){
		if(this._cbs[this.events[i][0]]){

			var num = this.events[i].length;

			if(num === 1){
				this._cbs[this.events[i][0]]();
			} else if(num === 2){
				this._cbs[this.events[i][0]](this.events[i][1]);
			} else {
				this._cbs[this.events[i][0]](this.events[i][1], this.events[i][2]);
			}
		}
	}
};
},{"./":37}],27:[function(require,module,exports){
var index = require("./index.js"),
    DomHandler = index.DomHandler,
	DomUtils = index.DomUtils;

//TODO: make this a streamable handler
function FeedHandler(callback, options){
	this.init(callback, options);
}

require("util").inherits(FeedHandler, DomHandler);

FeedHandler.prototype.init = DomHandler;

function getElements(what, where){
	return DomUtils.getElementsByTagName(what, where, true);
}
function getOneElement(what, where){
	return DomUtils.getElementsByTagName(what, where, true, 1)[0];
}
function fetch(what, where, recurse){
	return DomUtils.getText(
		DomUtils.getElementsByTagName(what, where, recurse, 1)
	).trim();
}

function addConditionally(obj, prop, what, where, recurse){
	var tmp = fetch(what, where, recurse);
	if(tmp) obj[prop] = tmp;
}

var isValidFeed = function(value) {
	return value === "rss" || value === "feed" || value === "rdf:RDF";
};

FeedHandler.prototype.onend = function() {
	var feed = {},
		feedRoot = getOneElement(isValidFeed, this.dom),
		tmp, childs;

	if (feedRoot) {
		if(feedRoot.name === "feed"){
			childs = feedRoot.children;

			feed.type = "atom";
			addConditionally(feed, "id", "id", childs);
			addConditionally(feed, "title", "title", childs);
			if((tmp = getOneElement("link", childs)) && (tmp = tmp.attribs) && (tmp = tmp.href)) feed.link = tmp;
			addConditionally(feed, "description", "subtitle", childs);
			if(tmp = fetch("updated", childs)) feed.updated = new Date(tmp);
			addConditionally(feed, "author", "email", childs, true);

			feed.items = getElements("entry", childs).map(function(item){
				var entry = {}, tmp;

				item = item.children;

				addConditionally(entry, "id", "id", item);
				addConditionally(entry, "title", "title", item);
				if((tmp = getOneElement("link", item)) && (tmp = tmp.attribs) && (tmp = tmp.href)) entry.link = tmp;
				addConditionally(entry, "description", "summary", item);
				if(tmp = fetch("updated", item)) entry.pubDate = new Date(tmp);
				return entry;
			});
		} else{
			childs = getOneElement("channel", feedRoot.children).children;

			feed.type = feedRoot.name.substr(0, 3);
			feed.id = "";
			addConditionally(feed, "title", "title", childs);
			addConditionally(feed, "link", "link", childs);
			addConditionally(feed, "description", "description", childs);
			if(tmp = fetch("lastBuildDate", childs)) feed.updated = new Date(tmp);
			addConditionally(feed, "author", "managingEditor", childs, true);

			feed.items = getElements("item", feedRoot.children).map(function(item){
				var entry = {}, tmp;

				item = item.children;

				addConditionally(entry, "id", "guid", item);
				addConditionally(entry, "title", "title", item);
				addConditionally(entry, "link", "link", item);
				addConditionally(entry, "description", "description", item);
				if(tmp = fetch("pubDate", item)) entry.pubDate = new Date(tmp);
				return entry;
			});
		}
	}
	this.dom = feed;
	DomHandler.prototype._handleCallback.call(
		this, feedRoot ? null : Error("couldn't find root of feed")
	);
};

module.exports = FeedHandler;

},{"./index.js":37,"util":22}],28:[function(require,module,exports){
var Tokenizer = require("./Tokenizer.js");

/*
	Options:

	xmlMode: Special behavior for script/style tags (true by default)
	lowerCaseAttributeNames: call .toLowerCase for each attribute name (true if xmlMode is `false`)
	lowerCaseTags: call .toLowerCase for each tag name (true if xmlMode is `false`)
*/

/*
	Callbacks:

	oncdataend,
	oncdatastart,
	onclosetag,
	oncomment,
	oncommentend,
	onerror,
	onopentag,
	onprocessinginstruction,
	onreset,
	ontext
*/

var formTags = {
	input: true,
	option: true,
	optgroup: true,
	select: true,
	button: true,
	datalist: true,
	textarea: true
};

var openImpliesClose = {
	tr      : { tr:true, th:true, td:true },
	th      : { th:true },
	td      : { thead:true, td:true },
	body    : { head:true, link:true, script:true },
	li      : { li:true },
	p       : { p:true },
	select  : formTags,
	input   : formTags,
	output  : formTags,
	button  : formTags,
	datalist: formTags,
	textarea: formTags,
	option  : { option:true },
	optgroup: { optgroup:true }
};

var voidElements = {
	__proto__: null,
	area: true,
	base: true,
	basefont: true,
	br: true,
	col: true,
	command: true,
	embed: true,
	frame: true,
	hr: true,
	img: true,
	input: true,
	isindex: true,
	keygen: true,
	link: true,
	meta: true,
	param: true,
	source: true,
	track: true,
	wbr: true
};

var re_nameEnd = /\s|\//;

function Parser(cbs, options){
	this._options = options || {};
	this._cbs = cbs || {};

	this._tagname = "";
	this._attribname = "";
	this._attribvalue = "";
	this._attribs = null;
	this._stack = [];
	this._done = false;

	this.startIndex = 0;
	this.endIndex = null;

	this._tokenizer = new Tokenizer(options, this);
}

require("util").inherits(Parser, require("events").EventEmitter);

Parser.prototype._updatePosition = function(initialOffset){
	if(this.endIndex === null){
		this.startIndex = this._tokenizer._sectionStart <= initialOffset ? 0 : this._tokenizer._sectionStart - initialOffset;
	}
	this.startIndex = this.endIndex + 1;
	this.endIndex = this._tokenizer._index;
};

//Tokenizer event handlers
Parser.prototype.ontext = function(data){
	this._updatePosition(1);
	this.endIndex--;

	if(this._cbs.ontext) this._cbs.ontext(data);
};

Parser.prototype.onopentagname = function(name){
	if(!(this._options.xmlMode || "lowerCaseTags" in this._options) || this._options.lowerCaseTags){
		name = name.toLowerCase();
	}

	this._tagname = name;

	if (!this._options.xmlMode && name in openImpliesClose) {
		for(
			var el;
			(el = this._stack[this._stack.length-1]) in openImpliesClose[name];
			this.onclosetag(el)
		);
	}

	if(this._options.xmlMode || !(name in voidElements)){
		this._stack.push(name);
	}

	if(this._cbs.onopentagname) this._cbs.onopentagname(name);
	if(this._cbs.onopentag) this._attribs = {};
};

Parser.prototype.onopentagend = function(){
	this._updatePosition(1);
    
	if(this._attribs){
		if(this._cbs.onopentag) this._cbs.onopentag(this._tagname, this._attribs);
		this._attribs = null;
	}
    
	if(!this._options.xmlMode && this._cbs.onclosetag && this._tagname in voidElements){
		this._cbs.onclosetag(this._tagname);
	}
    
	this._tagname = "";
};

Parser.prototype.onclosetag = function(name){
	this._updatePosition(1);

	if(!(this._options.xmlMode || "lowerCaseTags" in this._options) || this._options.lowerCaseTags){
		name = name.toLowerCase();
	}

	if(this._stack.length && (!(name in voidElements) || this._options.xmlMode)){
		var pos = this._stack.lastIndexOf(name);
		if(pos !== -1){
			if(this._cbs.onclosetag){
				pos = this._stack.length - pos;
				while(pos--) this._cbs.onclosetag(this._stack.pop());
			}
			else this._stack.length = pos;
		} else if(name === "p" && !this._options.xmlMode){
			this.onopentagname(name);
			this._closeCurrentTag();
		}
	} else if(!this._options.xmlMode && (name === "br" || name === "p")){
		this.onopentagname(name);
		this._closeCurrentTag();
	}
};

Parser.prototype.onselfclosingtag = function(){
	if(this._options.xmlMode){
		this._closeCurrentTag();
	} else {
		this.onopentagend();
	}
};

Parser.prototype._closeCurrentTag = function(){
	var name = this._tagname;

	this.onopentagend();

	//self-closing tags will be on the top of the stack
	//(cheaper check than in onclosetag)
	if(this._stack[this._stack.length-1] === name){
		if(this._cbs.onclosetag){
			this._cbs.onclosetag(name);
		}
		this._stack.pop();
	}
};

Parser.prototype.onattribname = function(name){
	if(!(this._options.xmlMode || "lowerCaseAttributeNames" in this._options) || this._options.lowerCaseAttributeNames){
		name = name.toLowerCase();
	}
	this._attribname = name;
};

Parser.prototype.onattribdata = function(value){
	this._attribvalue += value;
};

Parser.prototype.onattribend = function(){
	if(this._cbs.onattribute) this._cbs.onattribute(this._attribname, this._attribvalue);
	if(
		this._attribs &&
		!Object.prototype.hasOwnProperty.call(this._attribs, this._attribname)
	){
		this._attribs[this._attribname] = this._attribvalue;
	}
	this._attribname = "";
	this._attribvalue = "";
};

Parser.prototype.ondeclaration = function(value){
	if(this._cbs.onprocessinginstruction){
		var idx = value.search(re_nameEnd),
		    name = idx < 0 ? value : value.substr(0, idx);

		if(!(this._options.xmlMode || "lowerCaseTags" in this._options) || this._options.lowerCaseTags){
			name = name.toLowerCase();
		}
		this._cbs.onprocessinginstruction("!" + name, "!" + value);
	}
};

Parser.prototype.onprocessinginstruction = function(value){
	if(this._cbs.onprocessinginstruction){
		var idx = value.search(re_nameEnd),
		    name = idx < 0 ? value : value.substr(0, idx);

		if(!(this._options.xmlMode || "lowerCaseTags" in this._options) || this._options.lowerCaseTags){
			name = name.toLowerCase();
		}
		this._cbs.onprocessinginstruction("?" + name, "?" + value);
	}
};

Parser.prototype.oncomment = function(value){
	this._updatePosition(4);

	if(this._cbs.oncomment) this._cbs.oncomment(value);
	if(this._cbs.oncommentend) this._cbs.oncommentend();
};

Parser.prototype.oncdata = function(value){
	this._updatePosition(1);

	if(this._options.xmlMode){
		if(this._cbs.oncdatastart) this._cbs.oncdatastart();
		if(this._cbs.ontext) this._cbs.ontext(value);
		if(this._cbs.oncdataend) this._cbs.oncdataend();
	} else {
		this.oncomment("[CDATA[" + value + "]]");
	}
};

Parser.prototype.onerror = function(err){
	if(this._cbs.onerror) this._cbs.onerror(err);
};

Parser.prototype.onend = function(){
	if(this._cbs.onclosetag){
		for(
			var i = this._stack.length;
			i > 0;
			this._cbs.onclosetag(this._stack[--i])
		);
	}
	if(this._cbs.onend) this._cbs.onend();
};


//Resets the parser to a blank state, ready to parse a new HTML document
Parser.prototype.reset = function(){
	if(this._cbs.onreset) this._cbs.onreset();
	this._tokenizer.reset();

	this._tagname = "";
	this._attribname = "";
	this._attribs = null;
	this._stack = [];
	this._done = false;
};

//Parses a complete HTML document and pushes it to the handler
Parser.prototype.parseComplete = function(data){
	this.reset();
	this.end(data);
};

Parser.prototype.write = function(chunk){
	if(this._done) this.onerror(Error(".write() after done!"));
	this._tokenizer.write(chunk);
};

Parser.prototype.end = function(chunk){
	if(this._done) this.onerror(Error(".end() after done!"));
	this._tokenizer.end(chunk);
	this._done = true;
};

//alias for backwards compat
Parser.prototype.parseChunk = Parser.prototype.write;
Parser.prototype.done = Parser.prototype.end;

module.exports = Parser;

},{"./Tokenizer.js":31,"events":10,"util":22}],29:[function(require,module,exports){
module.exports = ProxyHandler;

var ProxyHandler = function(cbs){
	this._cbs = cbs || {};
};

var EVENTS = require("./").EVENTS;
Object.keys(EVENTS).forEach(function(name){
	if(EVENTS[name] === 0){
		name = "on" + name;
		ProxyHandler.prototype[name] = function(){
			if(this._cbs[name]) this._cbs[name]();
		};
	} else if(EVENTS[name] === 1){
		name = "on" + name;
		ProxyHandler.prototype[name] = function(a){
			if(this._cbs[name]) this._cbs[name](a);
		};
	} else if(EVENTS[name] === 2){
		name = "on" + name;
		ProxyHandler.prototype[name] = function(a, b){
			if(this._cbs[name]) this._cbs[name](a, b);
		};
	} else {
		throw Error("wrong number of arguments");
	}
});
},{"./":37}],30:[function(require,module,exports){
module.exports = Stream;

var Parser = require("./WritableStream.js");

function Stream(options){
	Parser.call(this, new Cbs(this), options);
}

require("util").inherits(Stream, Parser);

Stream.prototype.readable = true;

function Cbs(scope){
	this.scope = scope;
}

var EVENTS = require("../").EVENTS;

Object.keys(EVENTS).forEach(function(name){
	if(EVENTS[name] === 0){
		Cbs.prototype["on" + name] = function(){
			this.scope.emit(name);
		};
	} else if(EVENTS[name] === 1){
		Cbs.prototype["on" + name] = function(a){
			this.scope.emit(name, a);
		};
	} else if(EVENTS[name] === 2){
		Cbs.prototype["on" + name] = function(a, b){
			this.scope.emit(name, a, b);
		};
	} else {
		throw Error("wrong number of arguments!");
	}
});
},{"../":37,"./WritableStream.js":32,"util":22}],31:[function(require,module,exports){
module.exports = Tokenizer;

var entityMap = require("./entities/entities.json"),
    legacyMap = require("./entities/legacy.json"),
    xmlMap    = require("./entities/xml.json"),
    decodeMap = require("./entities/decode.json"),

    i = 0,

    TEXT                      = i++,
    BEFORE_TAG_NAME           = i++, //after <
    IN_TAG_NAME               = i++,
    IN_SELF_CLOSING_TAG       = i++,
    BEFORE_CLOSING_TAG_NAME   = i++,
    IN_CLOSING_TAG_NAME       = i++,
    AFTER_CLOSING_TAG_NAME    = i++,

    //attributes
    BEFORE_ATTRIBUTE_NAME     = i++,
    IN_ATTRIBUTE_NAME         = i++,
    AFTER_ATTRIBUTE_NAME      = i++,
    BEFORE_ATTRIBUTE_VALUE    = i++,
    IN_ATTRIBUTE_VALUE_DQ     = i++, // "
    IN_ATTRIBUTE_VALUE_SQ     = i++, // '
    IN_ATTRIBUTE_VALUE_NQ     = i++,

    //declarations
    BEFORE_DECLARATION        = i++, // !
    IN_DECLARATION            = i++,

    //processing instructions
    IN_PROCESSING_INSTRUCTION = i++, // ?

    //comments
    BEFORE_COMMENT            = i++,
    IN_COMMENT                = i++,
    AFTER_COMMENT_1           = i++,
    AFTER_COMMENT_2           = i++,

    //cdata
    BEFORE_CDATA_1            = i++, // [
    BEFORE_CDATA_2            = i++, // C
    BEFORE_CDATA_3            = i++, // D
    BEFORE_CDATA_4            = i++, // A
    BEFORE_CDATA_5            = i++, // T
    BEFORE_CDATA_6            = i++, // A
    IN_CDATA                  = i++,// [
    AFTER_CDATA_1             = i++, // ]
    AFTER_CDATA_2             = i++, // ]

    //special tags
    BEFORE_SPECIAL            = i++, //S
    BEFORE_SPECIAL_END        = i++,   //S

    BEFORE_SCRIPT_1           = i++, //C
    BEFORE_SCRIPT_2           = i++, //R
    BEFORE_SCRIPT_3           = i++, //I
    BEFORE_SCRIPT_4           = i++, //P
    BEFORE_SCRIPT_5           = i++, //T
    AFTER_SCRIPT_1            = i++, //C
    AFTER_SCRIPT_2            = i++, //R
    AFTER_SCRIPT_3            = i++, //I
    AFTER_SCRIPT_4            = i++, //P
    AFTER_SCRIPT_5            = i++, //T

    BEFORE_STYLE_1            = i++, //T
    BEFORE_STYLE_2            = i++, //Y
    BEFORE_STYLE_3            = i++, //L
    BEFORE_STYLE_4            = i++, //E
    AFTER_STYLE_1             = i++, //T
    AFTER_STYLE_2             = i++, //Y
    AFTER_STYLE_3             = i++, //L
    AFTER_STYLE_4             = i++, //E

    BEFORE_ENTITY             = i++, //&
    BEFORE_NUMERIC_ENTITY     = i++, //#
    IN_NAMED_ENTITY           = i++,
    IN_NUMERIC_ENTITY         = i++,
    IN_HEX_ENTITY             = i++, //X

    j = 0,

    SPECIAL_NONE              = j++,
    SPECIAL_SCRIPT            = j++,
    SPECIAL_STYLE             = j++;

function whitespace(c){
	return c === " " || c === "\n" || c === "\t" || c === "\f" || c === "\r";
}

function ifElseState(upper, SUCCESS, FAILURE){
	var lower = upper.toLowerCase();

	if(upper === lower){
		return function(c){
			this._state = c === lower ? SUCCESS : FAILURE;
		};
	} else {
		return function(c){
			this._state = (c === lower || c === upper) ? SUCCESS : FAILURE;
		};
	}
}

function consumeSpecialNameChar(upper, NEXT_STATE){
	var lower = upper.toLowerCase();

	return function(c){
		if(c === lower || c === upper){
			this._state = NEXT_STATE;
		} else {
			this._state = IN_TAG_NAME;
			this._index--; //consume the token again
		}
	};
}

function Tokenizer(options, cbs){
	this._state = TEXT;
	this._buffer = "";
	this._sectionStart = 0;
	this._index = 0;
	this._baseState = TEXT;
	this._special = SPECIAL_NONE;
	this._cbs = cbs;
	this._running = true;
	this._xmlMode = !!(options && options.xmlMode);
	this._decodeEntities = !!(options && options.decodeEntities);
}

Tokenizer.prototype._stateText = function(c){
	if(c === "<"){
		if(this._index > this._sectionStart){
			this._cbs.ontext(this._getSection());
		}
		this._state = BEFORE_TAG_NAME;
		this._sectionStart = this._index;
	} else if(this._decodeEntities && this._special === SPECIAL_NONE && c === "&"){
		if(this._index > this._sectionStart){
			this._cbs.ontext(this._getSection());
		}
		this._baseState = TEXT;
		this._state = BEFORE_ENTITY;
		this._sectionStart = this._index;
	}
};

Tokenizer.prototype._stateBeforeTagName = function(c){
	if(c === "/"){
		this._state = BEFORE_CLOSING_TAG_NAME;
	} else if(c === ">" || this._special !== SPECIAL_NONE || whitespace(c)) {
		this._state = TEXT;
	} else if(c === "!"){
		this._state = BEFORE_DECLARATION;
		this._sectionStart = this._index + 1;
	} else if(c === "?"){
		this._state = IN_PROCESSING_INSTRUCTION;
		this._sectionStart = this._index + 1;
	} else if(c === "<"){
		this._cbs.ontext(this._getSection());
		this._sectionStart = this._index;
	} else {
		this._state = (!this._xmlMode && (c === "s" || c === "S")) ?
						BEFORE_SPECIAL : IN_TAG_NAME;
		this._sectionStart = this._index;
	}
};

Tokenizer.prototype._stateInTagName = function(c){
	if(c === "/" || c === ">" || whitespace(c)){
		this._emitToken("onopentagname");
		this._state = BEFORE_ATTRIBUTE_NAME;
		this._index--;
	}
};

Tokenizer.prototype._stateBeforeCloseingTagName = function(c){
	if(whitespace(c));
	else if(c === ">"){
		this._state = TEXT;
	} else if(this._special !== SPECIAL_NONE){
		if(c === "s" || c === "S"){
			this._state = BEFORE_SPECIAL_END;
		} else {
			this._state = TEXT;
			this._index--;
		}
	} else {
		this._state = IN_CLOSING_TAG_NAME;
		this._sectionStart = this._index;
	}
};

Tokenizer.prototype._stateInCloseingTagName = function(c){
	if(c === ">" || whitespace(c)){
		this._emitToken("onclosetag");
		this._state = AFTER_CLOSING_TAG_NAME;
		this._index--;
	}
};

Tokenizer.prototype._stateAfterCloseingTagName = function(c){
	//skip everything until ">"
	if(c === ">"){
		this._state = TEXT;
		this._sectionStart = this._index + 1;
	}
};

Tokenizer.prototype._stateBeforeAttributeName = function(c){
	if(c === ">"){
		this._cbs.onopentagend();
		this._state = TEXT;
		this._sectionStart = this._index + 1;
	} else if(c === "/"){
		this._state = IN_SELF_CLOSING_TAG;
	} else if(!whitespace(c)){
		this._state = IN_ATTRIBUTE_NAME;
		this._sectionStart = this._index;
	}
};

Tokenizer.prototype._stateInSelfClosingTag = function(c){
	if(c === ">"){
		this._cbs.onselfclosingtag();
		this._state = TEXT;
		this._sectionStart = this._index + 1;
	} else if(!whitespace(c)){
		this._state = BEFORE_ATTRIBUTE_NAME;
		this._index--;
	}
};

Tokenizer.prototype._stateInAttributeName = function(c){
	if(c === "=" || c === "/" || c === ">" || whitespace(c)){
		if(this._index > this._sectionStart){
			this._cbs.onattribname(this._getSection());
		}
		this._sectionStart = -1;
		this._state = AFTER_ATTRIBUTE_NAME;
		this._index--;
	}
};

Tokenizer.prototype._stateAfterAttributeName = function(c){
	if(c === "="){
		this._state = BEFORE_ATTRIBUTE_VALUE;
	} else if(c === "/" || c === ">"){
		this._cbs.onattribend();
		this._state = BEFORE_ATTRIBUTE_NAME;
		this._index--;
	} else if(!whitespace(c)){
		this._cbs.onattribend();
		this._state = IN_ATTRIBUTE_NAME;
		this._sectionStart = this._index;
	}
};

Tokenizer.prototype._stateBeforeAttributeValue = function(c){
	if(c === "\""){
		this._state = IN_ATTRIBUTE_VALUE_DQ;
		this._sectionStart = this._index + 1;
	} else if(c === "'"){
		this._state = IN_ATTRIBUTE_VALUE_SQ;
		this._sectionStart = this._index + 1;
	} else if(!whitespace(c)){
		this._state = IN_ATTRIBUTE_VALUE_NQ;
		this._sectionStart = this._index;
	}
};

Tokenizer.prototype._stateInAttributeValueDoubleQuotes = function(c){
	if(c === "\""){
		this._emitToken("onattribdata");
		this._cbs.onattribend();
		this._state = BEFORE_ATTRIBUTE_NAME;
	} else if(this._decodeEntities && c === "&"){
		this._emitToken("onattribdata");
		this._baseState = this._state;
		this._state = BEFORE_ENTITY;
		this._sectionStart = this._index;
	}
};

Tokenizer.prototype._stateInAttributeValueSingleQuotes = function(c){
	if(c === "'"){
		this._emitToken("onattribdata");
		this._cbs.onattribend();
		this._state = BEFORE_ATTRIBUTE_NAME;
	} else if(this._decodeEntities && c === "&"){
		this._emitToken("onattribdata");
		this._baseState = this._state;
		this._state = BEFORE_ENTITY;
		this._sectionStart = this._index;
	}
};

Tokenizer.prototype._stateInAttributeValueNoQuotes = function(c){
	if(whitespace(c) || c === ">"){
		this._emitToken("onattribdata");
		this._cbs.onattribend();
		this._state = BEFORE_ATTRIBUTE_NAME;
		this._index--;
	} else if(this._decodeEntities && c === "&"){
		this._emitToken("onattribdata");
		this._baseState = this._state;
		this._state = BEFORE_ENTITY;
		this._sectionStart = this._index;
	}
};

Tokenizer.prototype._stateBeforeDeclaration = function(c){
	this._state = c === "[" ? BEFORE_CDATA_1 :
					c === "-" ? BEFORE_COMMENT :
						IN_DECLARATION;
};

Tokenizer.prototype._stateInDeclaration = function(c){
	if(c === ">"){
		this._cbs.ondeclaration(this._getSection());
		this._state = TEXT;
		this._sectionStart = this._index + 1;
	}
};

Tokenizer.prototype._stateInProcessingInstruction = function(c){
	if(c === ">"){
		this._cbs.onprocessinginstruction(this._getSection());
		this._state = TEXT;
		this._sectionStart = this._index + 1;
	}
};

Tokenizer.prototype._stateBeforeComment = function(c){
	if(c === "-"){
		this._state = IN_COMMENT;
		this._sectionStart = this._index + 1;
	} else {
		this._state = IN_DECLARATION;
	}
};

Tokenizer.prototype._stateInComment = function(c){
	if(c === "-") this._state = AFTER_COMMENT_1;
};

Tokenizer.prototype._stateAfterComment1 = ifElseState("-", AFTER_COMMENT_2, IN_COMMENT);

Tokenizer.prototype._stateAfterComment2 = function(c){
	if(c === ">"){
		//remove 2 trailing chars
		this._cbs.oncomment(this._buffer.substring(this._sectionStart, this._index - 2));
		this._state = TEXT;
		this._sectionStart = this._index + 1;
	} else if(c !== "-"){
		this._state = IN_COMMENT;
	}
	// else: stay in AFTER_COMMENT_2 (`--->`)
};

Tokenizer.prototype._stateBeforeCdata1 = ifElseState("C", BEFORE_CDATA_2, IN_DECLARATION);
Tokenizer.prototype._stateBeforeCdata2 = ifElseState("D", BEFORE_CDATA_3, IN_DECLARATION);
Tokenizer.prototype._stateBeforeCdata3 = ifElseState("A", BEFORE_CDATA_4, IN_DECLARATION);
Tokenizer.prototype._stateBeforeCdata4 = ifElseState("T", BEFORE_CDATA_5, IN_DECLARATION);
Tokenizer.prototype._stateBeforeCdata5 = ifElseState("A", BEFORE_CDATA_6, IN_DECLARATION);

Tokenizer.prototype._stateBeforeCdata6 = function(c){
	if(c === "["){
		this._state = IN_CDATA;
		this._sectionStart = this._index + 1;
	} else {
		this._state = IN_DECLARATION;
	}
};

Tokenizer.prototype._stateInCdata = function(c){
	if(c === "]") this._state = AFTER_CDATA_1;
};

Tokenizer.prototype._stateAfterCdata1 = ifElseState("]", AFTER_CDATA_2, IN_CDATA);

Tokenizer.prototype._stateAfterCdata2 = function(c){
	if(c === ">"){
		//remove 2 trailing chars
		this._cbs.oncdata(this._buffer.substring(this._sectionStart, this._index - 2));
		this._state = TEXT;
		this._sectionStart = this._index + 1;
	} else if (c !== "]") {
		this._state = IN_CDATA;
	}
	//else: stay in AFTER_CDATA_2 (`]]]>`)
};

Tokenizer.prototype._stateBeforeSpecial = function(c){
	if(c === "c" || c === "C"){
		this._state = BEFORE_SCRIPT_1;
	} else if(c === "t" || c === "T"){
		this._state = BEFORE_STYLE_1;
	} else {
		this._state = IN_TAG_NAME;
		this._index--; //consume the token again
	}
};

Tokenizer.prototype._stateBeforeSpecialEnd = function(c){
	if(this._special === SPECIAL_SCRIPT && (c === "c" || c === "C")){
		this._state = AFTER_SCRIPT_1;
	} else if(this._special === SPECIAL_STYLE && (c === "t" || c === "T")){
		this._state = AFTER_STYLE_1;
	}
	else this._state = TEXT;
};

Tokenizer.prototype._stateBeforeScript1 = consumeSpecialNameChar("R", BEFORE_SCRIPT_2);
Tokenizer.prototype._stateBeforeScript2 = consumeSpecialNameChar("I", BEFORE_SCRIPT_3);
Tokenizer.prototype._stateBeforeScript3 = consumeSpecialNameChar("P", BEFORE_SCRIPT_4);
Tokenizer.prototype._stateBeforeScript4 = consumeSpecialNameChar("T", BEFORE_SCRIPT_5);

Tokenizer.prototype._stateBeforeScript5 = function(c){
	if(c === "/" || c === ">" || whitespace(c)){
		this._special = SPECIAL_SCRIPT;
	}
	this._state = IN_TAG_NAME;
	this._index--; //consume the token again
};

Tokenizer.prototype._stateAfterScript1 = ifElseState("R", AFTER_SCRIPT_2, TEXT);
Tokenizer.prototype._stateAfterScript2 = ifElseState("I", AFTER_SCRIPT_3, TEXT);
Tokenizer.prototype._stateAfterScript3 = ifElseState("P", AFTER_SCRIPT_4, TEXT);
Tokenizer.prototype._stateAfterScript4 = ifElseState("T", AFTER_SCRIPT_5, TEXT);

Tokenizer.prototype._stateAfterScript5 = function(c){
	if(c === ">" || whitespace(c)){
		this._special = SPECIAL_NONE;
		this._state = IN_CLOSING_TAG_NAME;
		this._sectionStart = this._index - 6;
		this._index--; //reconsume the token
	}
	else this._state = TEXT;
};

Tokenizer.prototype._stateBeforeStyle1 = consumeSpecialNameChar("Y", BEFORE_STYLE_2);
Tokenizer.prototype._stateBeforeStyle2 = consumeSpecialNameChar("L", BEFORE_STYLE_3);
Tokenizer.prototype._stateBeforeStyle3 = consumeSpecialNameChar("E", BEFORE_STYLE_4);

Tokenizer.prototype._stateBeforeStyle4 = function(c){
	if(c === "/" || c === ">" || whitespace(c)){
		this._special = SPECIAL_STYLE;
	}
	this._state = IN_TAG_NAME;
	this._index--; //consume the token again
};

Tokenizer.prototype._stateAfterStyle1 = ifElseState("Y", AFTER_STYLE_2, TEXT);
Tokenizer.prototype._stateAfterStyle2 = ifElseState("L", AFTER_STYLE_3, TEXT);
Tokenizer.prototype._stateAfterStyle3 = ifElseState("E", AFTER_STYLE_4, TEXT);

Tokenizer.prototype._stateAfterStyle4 = function(c){
	if(c === ">" || whitespace(c)){
		this._special = SPECIAL_NONE;
		this._state = IN_CLOSING_TAG_NAME;
		this._sectionStart = this._index - 5;
		this._index--; //reconsume the token
	}
	else this._state = TEXT;
};

Tokenizer.prototype._stateBeforeEntity = ifElseState("#", BEFORE_NUMERIC_ENTITY, IN_NAMED_ENTITY);
Tokenizer.prototype._stateBeforeNumericEntity = ifElseState("X", IN_HEX_ENTITY, IN_NUMERIC_ENTITY);

//for entities within attributes
Tokenizer.prototype._parseNamedEntityStrict = function(){
	//offset = 1
	if(this._sectionStart + 1 < this._index){
		var entity = this._buffer.substring(this._sectionStart + 1, this._index),
		    map = this._xmlMode ? xmlMap : entityMap;

		if(map.hasOwnProperty(entity)){
			this._emitPartial(map[entity]);
			this._sectionStart = this._index + 1;
		}
	}
};


//parses legacy entities (without trailing semicolon)
Tokenizer.prototype._parseLegacyEntity = function(){
	var start = this._sectionStart + 1,
	    limit = this._index - start;

	if(limit > 6) limit = 6; //the max length of legacy entities is 6

	while(limit >= 2){ //the min length of legacy entities is 2
		var entity = this._buffer.substr(start, limit);

		if(legacyMap.hasOwnProperty(entity)){
			this._emitPartial(legacyMap[entity]);
			this._sectionStart += limit + 2;
			break;
		} else {
			limit--;
		}
	}
};

Tokenizer.prototype._stateInNamedEntity = function(c){
	if(c === ";"){
		this._parseNamedEntityStrict();
		if(this._sectionStart + 1 < this._index && !this._xmlMode){
			this._parseLegacyEntity();
		}
		this._state = this._baseState;
	} else if((c < "a" || c > "z") && (c < "A" || c > "Z") && (c < "0" || c > "9")){
		if(this._xmlMode);
		else if(this._baseState !== TEXT){
			if(c !== "="){
				this._parseNamedEntityStrict();
				this._sectionStart--; //include the current character in the section
			}
		} else {
			this._parseLegacyEntity();
			this._sectionStart--;
		}
		this._state = this._baseState;
		this._index--;
	}
};

// modified version of https://github.com/mathiasbynens/he/blob/master/src/he.js#L94-L119
function decodeCodePoint(codePoint){
	var output = "";

	if((codePoint >= 0xD800 && codePoint <= 0xDFFF) || codePoint > 0x10FFFF){
		return "\uFFFD";
	}

	if(codePoint in decodeMap){
		codePoint = decodeMap[codePoint];
	}

	if(codePoint > 0xFFFF){
		codePoint -= 0x10000;
		output += String.fromCharCode(codePoint >>> 10 & 0x3FF | 0xD800);
		codePoint = 0xDC00 | codePoint & 0x3FF;
	}

	output += String.fromCharCode(codePoint);
	return output;
}

Tokenizer.prototype._decodeNumericEntity = function(offset, base){
	var sectionStart = this._sectionStart + offset;

	if(sectionStart !== this._index){
		//parse entity
		var entity = this._buffer.substring(sectionStart, this._index);
		var parsed = parseInt(entity, base);

		if(parsed === parsed){ //not NaN (TODO: when can this happen?)
			this._emitPartial(decodeCodePoint(parsed));
			this._sectionStart = this._index;
		}
	}

	this._state = this._baseState;
};

Tokenizer.prototype._stateInNumericEntity = function(c){
	if(c === ";"){
		this._decodeNumericEntity(2, 10);
		this._sectionStart++;
	} else if(c < "0" || c > "9"){
		if(!this._xmlMode){
			this._decodeNumericEntity(2, 10);
		} else {
			this._state = this._baseState;
		}
		this._index--;
	}
};

Tokenizer.prototype._stateInHexEntity = function(c){
	if(c === ";"){
		this._decodeNumericEntity(3, 16);
		this._sectionStart++;
	} else if((c < "a" || c > "f") && (c < "A" || c > "F") && (c < "0" || c > "9")){
		if(!this._xmlMode){
			this._decodeNumericEntity(3, 16);
		} else {
			this._state = this._baseState;
		}
		this._index--;
	}
};

Tokenizer.prototype._cleanup = function () {
	if(this._sectionStart < 0){
		this._buffer = "";
		this._index = 0;
	} else {
		if(this._state === TEXT){
			if(this._sectionStart !== this._index){
				this._cbs.ontext(this._buffer.substr(this._sectionStart));
			}
			this._buffer = "";
			this._index = 0;
		} else if(this._sectionStart === this._index){
			//the section just started
			this._buffer = "";
			this._index = 0;
		} else {
			//remove everything unnecessary
			this._buffer = this._buffer.substr(this._sectionStart);
			this._index -= this._sectionStart;
		}

		this._sectionStart = 0;
	}
};

//TODO make events conditional
Tokenizer.prototype.write = function(chunk){
	this._buffer += chunk;

	while(this._index < this._buffer.length && this._running){
		var c = this._buffer.charAt(this._index);
		if(this._state === TEXT) {
			this._stateText(c);
		} else if(this._state === BEFORE_TAG_NAME){
			this._stateBeforeTagName(c);
		} else if(this._state === IN_TAG_NAME) {
			this._stateInTagName(c);
		} else if(this._state === BEFORE_CLOSING_TAG_NAME){
			this._stateBeforeCloseingTagName(c);
		} else if(this._state === IN_CLOSING_TAG_NAME){
			this._stateInCloseingTagName(c);
		} else if(this._state === AFTER_CLOSING_TAG_NAME){
			this._stateAfterCloseingTagName(c);
		} else if(this._state === IN_SELF_CLOSING_TAG){
			this._stateInSelfClosingTag(c);
		}

		/*
		*	attributes
		*/
		else if(this._state === BEFORE_ATTRIBUTE_NAME){
			this._stateBeforeAttributeName(c);
		} else if(this._state === IN_ATTRIBUTE_NAME){
			this._stateInAttributeName(c);
		} else if(this._state === AFTER_ATTRIBUTE_NAME){
			this._stateAfterAttributeName(c);
		} else if(this._state === BEFORE_ATTRIBUTE_VALUE){
			this._stateBeforeAttributeValue(c);
		} else if(this._state === IN_ATTRIBUTE_VALUE_DQ){
			this._stateInAttributeValueDoubleQuotes(c);
		} else if(this._state === IN_ATTRIBUTE_VALUE_SQ){
			this._stateInAttributeValueSingleQuotes(c);
		} else if(this._state === IN_ATTRIBUTE_VALUE_NQ){
			this._stateInAttributeValueNoQuotes(c);
		}

		/*
		*	declarations
		*/
		else if(this._state === BEFORE_DECLARATION){
			this._stateBeforeDeclaration(c);
		} else if(this._state === IN_DECLARATION){
			this._stateInDeclaration(c);
		}

		/*
		*	processing instructions
		*/
		else if(this._state === IN_PROCESSING_INSTRUCTION){
			this._stateInProcessingInstruction(c);
		}

		/*
		*	comments
		*/
		else if(this._state === BEFORE_COMMENT){
			this._stateBeforeComment(c);
		} else if(this._state === IN_COMMENT){
			this._stateInComment(c);
		} else if(this._state === AFTER_COMMENT_1){
			this._stateAfterComment1(c);
		} else if(this._state === AFTER_COMMENT_2){
			this._stateAfterComment2(c);
		}

		/*
		*	cdata
		*/
		else if(this._state === BEFORE_CDATA_1){
			this._stateBeforeCdata1(c);
		} else if(this._state === BEFORE_CDATA_2){
			this._stateBeforeCdata2(c);
		} else if(this._state === BEFORE_CDATA_3){
			this._stateBeforeCdata3(c);
		} else if(this._state === BEFORE_CDATA_4){
			this._stateBeforeCdata4(c);
		} else if(this._state === BEFORE_CDATA_5){
			this._stateBeforeCdata5(c);
		} else if(this._state === BEFORE_CDATA_6){
			this._stateBeforeCdata6(c);
		} else if(this._state === IN_CDATA){
			this._stateInCdata(c);
		} else if(this._state === AFTER_CDATA_1){
			this._stateAfterCdata1(c);
		} else if(this._state === AFTER_CDATA_2){
			this._stateAfterCdata2(c);
		}

		/*
		* special tags
		*/
		else if(this._state === BEFORE_SPECIAL){
			this._stateBeforeSpecial(c);
		} else if(this._state === BEFORE_SPECIAL_END){
			this._stateBeforeSpecialEnd(c);
		}

		/*
		* script
		*/
		else if(this._state === BEFORE_SCRIPT_1){
			this._stateBeforeScript1(c);
		} else if(this._state === BEFORE_SCRIPT_2){
			this._stateBeforeScript2(c);
		} else if(this._state === BEFORE_SCRIPT_3){
			this._stateBeforeScript3(c);
		} else if(this._state === BEFORE_SCRIPT_4){
			this._stateBeforeScript4(c);
		} else if(this._state === BEFORE_SCRIPT_5){
			this._stateBeforeScript5(c);
		}

		else if(this._state === AFTER_SCRIPT_1){
			this._stateAfterScript1(c);
		} else if(this._state === AFTER_SCRIPT_2){
			this._stateAfterScript2(c);
		} else if(this._state === AFTER_SCRIPT_3){
			this._stateAfterScript3(c);
		} else if(this._state === AFTER_SCRIPT_4){
			this._stateAfterScript4(c);
		} else if(this._state === AFTER_SCRIPT_5){
			this._stateAfterScript5(c);
		}

		/*
		* style
		*/
		else if(this._state === BEFORE_STYLE_1){
			this._stateBeforeStyle1(c);
		} else if(this._state === BEFORE_STYLE_2){
			this._stateBeforeStyle2(c);
		} else if(this._state === BEFORE_STYLE_3){
			this._stateBeforeStyle3(c);
		} else if(this._state === BEFORE_STYLE_4){
			this._stateBeforeStyle4(c);
		}

		else if(this._state === AFTER_STYLE_1){
			this._stateAfterStyle1(c);
		} else if(this._state === AFTER_STYLE_2){
			this._stateAfterStyle2(c);
		} else if(this._state === AFTER_STYLE_3){
			this._stateAfterStyle3(c);
		} else if(this._state === AFTER_STYLE_4){
			this._stateAfterStyle4(c);
		}

		/*
		* entities
		*/
		else if(this._state === BEFORE_ENTITY){
			this._stateBeforeEntity(c);
		} else if(this._state === BEFORE_NUMERIC_ENTITY){
			this._stateBeforeNumericEntity(c);
		} else if(this._state === IN_NAMED_ENTITY){
			this._stateInNamedEntity(c);
		} else if(this._state === IN_NUMERIC_ENTITY){
			this._stateInNumericEntity(c);
		} else if(this._state === IN_HEX_ENTITY){
			this._stateInHexEntity(c);
		}

		else {
			this._cbs.onerror(Error("unknown _state"), this._state);
		}

		this._index++;
	}

	this._cleanup();
};

Tokenizer.prototype.pause = function(){
	this._running = false;
};
Tokenizer.prototype.resume = function(){
	this._running = true;
};

Tokenizer.prototype.end = function(chunk){
	if(chunk) this.write(chunk);

	//if there is remaining data, emit it in a reasonable way
	if(this._sectionStart < this._index){
		this._handleTrailingData();
	}

	this._cbs.onend();
};

Tokenizer.prototype._handleTrailingData = function(){
	var data = this._buffer.substr(this._sectionStart);

	if(this._state === IN_CDATA || this._state === AFTER_CDATA_1 || this._state === AFTER_CDATA_2){
		this._cbs.oncdata(data);
	} else if(this._state === IN_COMMENT || this._state === AFTER_COMMENT_1 || this._state === AFTER_COMMENT_2){
		this._cbs.oncomment(data);
	} else if(this._state === IN_TAG_NAME){
		this._cbs.onopentagname(data);
	} else if(this._state === BEFORE_ATTRIBUTE_NAME || this._state === BEFORE_ATTRIBUTE_VALUE || this._state === AFTER_ATTRIBUTE_NAME){
		this._cbs.onopentagend();
	} else if(this._state === IN_ATTRIBUTE_NAME){
		this._cbs.onattribname(data);
	} else if(this._state === IN_ATTRIBUTE_VALUE_SQ || this._state === IN_ATTRIBUTE_VALUE_DQ || this._state === IN_ATTRIBUTE_VALUE_NQ){
		this._cbs.onattribdata(data);
		this._cbs.onattribend();
	} else if(this._state === IN_CLOSING_TAG_NAME){
		this._cbs.onclosetag(data);
	} else if(this._state === IN_NAMED_ENTITY && !this._xmlMode){
		this._parseLegacyEntity();
		if(--this._sectionStart < this._index){
			this._state = this._baseState;
			this._handleTrailingData();
		}
	} else if(this._state === IN_NUMERIC_ENTITY && !this._xmlMode){
		this._decodeNumericEntity(2, 10);
		if(this._sectionStart < this._index){
			this._state = this._baseState;
			this._handleTrailingData();
		}
	} else if(this._state === IN_HEX_ENTITY && !this._xmlMode){
		this._decodeNumericEntity(3, 16);
		if(this._sectionStart < this._index){
			this._state = this._baseState;
			this._handleTrailingData();
		}
	} else {
		this._cbs.ontext(data);
	}
};

Tokenizer.prototype.reset = function(){
	Tokenizer.call(this, {xmlMode: this._xmlMode, decodeEntities: this._decodeEntities}, this._cbs);
};

Tokenizer.prototype._getSection = function(){
	return this._buffer.substring(this._sectionStart, this._index);
};

Tokenizer.prototype._emitToken = function(name){
	this._cbs[name](this._getSection());
	this._sectionStart = -1;
};

Tokenizer.prototype._emitPartial = function(value){
	if(this._baseState !== TEXT){
		this._cbs.onattribdata(value); //TODO implement the new event
	} else {
		this._cbs.ontext(value);
	}
};

},{"./entities/decode.json":33,"./entities/entities.json":34,"./entities/legacy.json":35,"./entities/xml.json":36}],32:[function(require,module,exports){
module.exports = Stream;

var Parser = require("./Parser.js"),
    WritableStream = require("stream").Writable || require("readable-stream").Writable;

function Stream(cbs, options){
	var parser = this._parser = new Parser(cbs, options);

	WritableStream.call(this, {decodeStrings: false});

	this.once("finish", function(){
		parser.end();
	});
}

require("util").inherits(Stream, WritableStream);

WritableStream.prototype._write = function(chunk, encoding, cb){
	this._parser.write(chunk);
	cb();
};
},{"./Parser.js":28,"readable-stream":50,"stream":14,"util":22}],33:[function(require,module,exports){
module.exports={"0":"\uFFFD","128":"\u20AC","130":"\u201A","131":"\u0192","132":"\u201E","133":"\u2026","134":"\u2020","135":"\u2021","136":"\u02C6","137":"\u2030","138":"\u0160","139":"\u2039","140":"\u0152","142":"\u017D","145":"\u2018","146":"\u2019","147":"\u201C","148":"\u201D","149":"\u2022","150":"\u2013","151":"\u2014","152":"\u02DC","153":"\u2122","154":"\u0161","155":"\u203A","156":"\u0153","158":"\u017E","159":"\u0178"}

},{}],34:[function(require,module,exports){
module.exports={"Aacute":"\u00C1","aacute":"\u00E1","Abreve":"\u0102","abreve":"\u0103","ac":"\u223E","acd":"\u223F","acE":"\u223E\u0333","Acirc":"\u00C2","acirc":"\u00E2","acute":"\u00B4","Acy":"\u0410","acy":"\u0430","AElig":"\u00C6","aelig":"\u00E6","af":"\u2061","Afr":"\uD835\uDD04","afr":"\uD835\uDD1E","Agrave":"\u00C0","agrave":"\u00E0","alefsym":"\u2135","aleph":"\u2135","Alpha":"\u0391","alpha":"\u03B1","Amacr":"\u0100","amacr":"\u0101","amalg":"\u2A3F","amp":"&","AMP":"&","andand":"\u2A55","And":"\u2A53","and":"\u2227","andd":"\u2A5C","andslope":"\u2A58","andv":"\u2A5A","ang":"\u2220","ange":"\u29A4","angle":"\u2220","angmsdaa":"\u29A8","angmsdab":"\u29A9","angmsdac":"\u29AA","angmsdad":"\u29AB","angmsdae":"\u29AC","angmsdaf":"\u29AD","angmsdag":"\u29AE","angmsdah":"\u29AF","angmsd":"\u2221","angrt":"\u221F","angrtvb":"\u22BE","angrtvbd":"\u299D","angsph":"\u2222","angst":"\u00C5","angzarr":"\u237C","Aogon":"\u0104","aogon":"\u0105","Aopf":"\uD835\uDD38","aopf":"\uD835\uDD52","apacir":"\u2A6F","ap":"\u2248","apE":"\u2A70","ape":"\u224A","apid":"\u224B","apos":"'","ApplyFunction":"\u2061","approx":"\u2248","approxeq":"\u224A","Aring":"\u00C5","aring":"\u00E5","Ascr":"\uD835\uDC9C","ascr":"\uD835\uDCB6","Assign":"\u2254","ast":"*","asymp":"\u2248","asympeq":"\u224D","Atilde":"\u00C3","atilde":"\u00E3","Auml":"\u00C4","auml":"\u00E4","awconint":"\u2233","awint":"\u2A11","backcong":"\u224C","backepsilon":"\u03F6","backprime":"\u2035","backsim":"\u223D","backsimeq":"\u22CD","Backslash":"\u2216","Barv":"\u2AE7","barvee":"\u22BD","barwed":"\u2305","Barwed":"\u2306","barwedge":"\u2305","bbrk":"\u23B5","bbrktbrk":"\u23B6","bcong":"\u224C","Bcy":"\u0411","bcy":"\u0431","bdquo":"\u201E","becaus":"\u2235","because":"\u2235","Because":"\u2235","bemptyv":"\u29B0","bepsi":"\u03F6","bernou":"\u212C","Bernoullis":"\u212C","Beta":"\u0392","beta":"\u03B2","beth":"\u2136","between":"\u226C","Bfr":"\uD835\uDD05","bfr":"\uD835\uDD1F","bigcap":"\u22C2","bigcirc":"\u25EF","bigcup":"\u22C3","bigodot":"\u2A00","bigoplus":"\u2A01","bigotimes":"\u2A02","bigsqcup":"\u2A06","bigstar":"\u2605","bigtriangledown":"\u25BD","bigtriangleup":"\u25B3","biguplus":"\u2A04","bigvee":"\u22C1","bigwedge":"\u22C0","bkarow":"\u290D","blacklozenge":"\u29EB","blacksquare":"\u25AA","blacktriangle":"\u25B4","blacktriangledown":"\u25BE","blacktriangleleft":"\u25C2","blacktriangleright":"\u25B8","blank":"\u2423","blk12":"\u2592","blk14":"\u2591","blk34":"\u2593","block":"\u2588","bne":"=\u20E5","bnequiv":"\u2261\u20E5","bNot":"\u2AED","bnot":"\u2310","Bopf":"\uD835\uDD39","bopf":"\uD835\uDD53","bot":"\u22A5","bottom":"\u22A5","bowtie":"\u22C8","boxbox":"\u29C9","boxdl":"\u2510","boxdL":"\u2555","boxDl":"\u2556","boxDL":"\u2557","boxdr":"\u250C","boxdR":"\u2552","boxDr":"\u2553","boxDR":"\u2554","boxh":"\u2500","boxH":"\u2550","boxhd":"\u252C","boxHd":"\u2564","boxhD":"\u2565","boxHD":"\u2566","boxhu":"\u2534","boxHu":"\u2567","boxhU":"\u2568","boxHU":"\u2569","boxminus":"\u229F","boxplus":"\u229E","boxtimes":"\u22A0","boxul":"\u2518","boxuL":"\u255B","boxUl":"\u255C","boxUL":"\u255D","boxur":"\u2514","boxuR":"\u2558","boxUr":"\u2559","boxUR":"\u255A","boxv":"\u2502","boxV":"\u2551","boxvh":"\u253C","boxvH":"\u256A","boxVh":"\u256B","boxVH":"\u256C","boxvl":"\u2524","boxvL":"\u2561","boxVl":"\u2562","boxVL":"\u2563","boxvr":"\u251C","boxvR":"\u255E","boxVr":"\u255F","boxVR":"\u2560","bprime":"\u2035","breve":"\u02D8","Breve":"\u02D8","brvbar":"\u00A6","bscr":"\uD835\uDCB7","Bscr":"\u212C","bsemi":"\u204F","bsim":"\u223D","bsime":"\u22CD","bsolb":"\u29C5","bsol":"\\","bsolhsub":"\u27C8","bull":"\u2022","bullet":"\u2022","bump":"\u224E","bumpE":"\u2AAE","bumpe":"\u224F","Bumpeq":"\u224E","bumpeq":"\u224F","Cacute":"\u0106","cacute":"\u0107","capand":"\u2A44","capbrcup":"\u2A49","capcap":"\u2A4B","cap":"\u2229","Cap":"\u22D2","capcup":"\u2A47","capdot":"\u2A40","CapitalDifferentialD":"\u2145","caps":"\u2229\uFE00","caret":"\u2041","caron":"\u02C7","Cayleys":"\u212D","ccaps":"\u2A4D","Ccaron":"\u010C","ccaron":"\u010D","Ccedil":"\u00C7","ccedil":"\u00E7","Ccirc":"\u0108","ccirc":"\u0109","Cconint":"\u2230","ccups":"\u2A4C","ccupssm":"\u2A50","Cdot":"\u010A","cdot":"\u010B","cedil":"\u00B8","Cedilla":"\u00B8","cemptyv":"\u29B2","cent":"\u00A2","centerdot":"\u00B7","CenterDot":"\u00B7","cfr":"\uD835\uDD20","Cfr":"\u212D","CHcy":"\u0427","chcy":"\u0447","check":"\u2713","checkmark":"\u2713","Chi":"\u03A7","chi":"\u03C7","circ":"\u02C6","circeq":"\u2257","circlearrowleft":"\u21BA","circlearrowright":"\u21BB","circledast":"\u229B","circledcirc":"\u229A","circleddash":"\u229D","CircleDot":"\u2299","circledR":"\u00AE","circledS":"\u24C8","CircleMinus":"\u2296","CirclePlus":"\u2295","CircleTimes":"\u2297","cir":"\u25CB","cirE":"\u29C3","cire":"\u2257","cirfnint":"\u2A10","cirmid":"\u2AEF","cirscir":"\u29C2","ClockwiseContourIntegral":"\u2232","CloseCurlyDoubleQuote":"\u201D","CloseCurlyQuote":"\u2019","clubs":"\u2663","clubsuit":"\u2663","colon":":","Colon":"\u2237","Colone":"\u2A74","colone":"\u2254","coloneq":"\u2254","comma":",","commat":"@","comp":"\u2201","compfn":"\u2218","complement":"\u2201","complexes":"\u2102","cong":"\u2245","congdot":"\u2A6D","Congruent":"\u2261","conint":"\u222E","Conint":"\u222F","ContourIntegral":"\u222E","copf":"\uD835\uDD54","Copf":"\u2102","coprod":"\u2210","Coproduct":"\u2210","copy":"\u00A9","COPY":"\u00A9","copysr":"\u2117","CounterClockwiseContourIntegral":"\u2233","crarr":"\u21B5","cross":"\u2717","Cross":"\u2A2F","Cscr":"\uD835\uDC9E","cscr":"\uD835\uDCB8","csub":"\u2ACF","csube":"\u2AD1","csup":"\u2AD0","csupe":"\u2AD2","ctdot":"\u22EF","cudarrl":"\u2938","cudarrr":"\u2935","cuepr":"\u22DE","cuesc":"\u22DF","cularr":"\u21B6","cularrp":"\u293D","cupbrcap":"\u2A48","cupcap":"\u2A46","CupCap":"\u224D","cup":"\u222A","Cup":"\u22D3","cupcup":"\u2A4A","cupdot":"\u228D","cupor":"\u2A45","cups":"\u222A\uFE00","curarr":"\u21B7","curarrm":"\u293C","curlyeqprec":"\u22DE","curlyeqsucc":"\u22DF","curlyvee":"\u22CE","curlywedge":"\u22CF","curren":"\u00A4","curvearrowleft":"\u21B6","curvearrowright":"\u21B7","cuvee":"\u22CE","cuwed":"\u22CF","cwconint":"\u2232","cwint":"\u2231","cylcty":"\u232D","dagger":"\u2020","Dagger":"\u2021","daleth":"\u2138","darr":"\u2193","Darr":"\u21A1","dArr":"\u21D3","dash":"\u2010","Dashv":"\u2AE4","dashv":"\u22A3","dbkarow":"\u290F","dblac":"\u02DD","Dcaron":"\u010E","dcaron":"\u010F","Dcy":"\u0414","dcy":"\u0434","ddagger":"\u2021","ddarr":"\u21CA","DD":"\u2145","dd":"\u2146","DDotrahd":"\u2911","ddotseq":"\u2A77","deg":"\u00B0","Del":"\u2207","Delta":"\u0394","delta":"\u03B4","demptyv":"\u29B1","dfisht":"\u297F","Dfr":"\uD835\uDD07","dfr":"\uD835\uDD21","dHar":"\u2965","dharl":"\u21C3","dharr":"\u21C2","DiacriticalAcute":"\u00B4","DiacriticalDot":"\u02D9","DiacriticalDoubleAcute":"\u02DD","DiacriticalGrave":"`","DiacriticalTilde":"\u02DC","diam":"\u22C4","diamond":"\u22C4","Diamond":"\u22C4","diamondsuit":"\u2666","diams":"\u2666","die":"\u00A8","DifferentialD":"\u2146","digamma":"\u03DD","disin":"\u22F2","div":"\u00F7","divide":"\u00F7","divideontimes":"\u22C7","divonx":"\u22C7","DJcy":"\u0402","djcy":"\u0452","dlcorn":"\u231E","dlcrop":"\u230D","dollar":"$","Dopf":"\uD835\uDD3B","dopf":"\uD835\uDD55","Dot":"\u00A8","dot":"\u02D9","DotDot":"\u20DC","doteq":"\u2250","doteqdot":"\u2251","DotEqual":"\u2250","dotminus":"\u2238","dotplus":"\u2214","dotsquare":"\u22A1","doublebarwedge":"\u2306","DoubleContourIntegral":"\u222F","DoubleDot":"\u00A8","DoubleDownArrow":"\u21D3","DoubleLeftArrow":"\u21D0","DoubleLeftRightArrow":"\u21D4","DoubleLeftTee":"\u2AE4","DoubleLongLeftArrow":"\u27F8","DoubleLongLeftRightArrow":"\u27FA","DoubleLongRightArrow":"\u27F9","DoubleRightArrow":"\u21D2","DoubleRightTee":"\u22A8","DoubleUpArrow":"\u21D1","DoubleUpDownArrow":"\u21D5","DoubleVerticalBar":"\u2225","DownArrowBar":"\u2913","downarrow":"\u2193","DownArrow":"\u2193","Downarrow":"\u21D3","DownArrowUpArrow":"\u21F5","DownBreve":"\u0311","downdownarrows":"\u21CA","downharpoonleft":"\u21C3","downharpoonright":"\u21C2","DownLeftRightVector":"\u2950","DownLeftTeeVector":"\u295E","DownLeftVectorBar":"\u2956","DownLeftVector":"\u21BD","DownRightTeeVector":"\u295F","DownRightVectorBar":"\u2957","DownRightVector":"\u21C1","DownTeeArrow":"\u21A7","DownTee":"\u22A4","drbkarow":"\u2910","drcorn":"\u231F","drcrop":"\u230C","Dscr":"\uD835\uDC9F","dscr":"\uD835\uDCB9","DScy":"\u0405","dscy":"\u0455","dsol":"\u29F6","Dstrok":"\u0110","dstrok":"\u0111","dtdot":"\u22F1","dtri":"\u25BF","dtrif":"\u25BE","duarr":"\u21F5","duhar":"\u296F","dwangle":"\u29A6","DZcy":"\u040F","dzcy":"\u045F","dzigrarr":"\u27FF","Eacute":"\u00C9","eacute":"\u00E9","easter":"\u2A6E","Ecaron":"\u011A","ecaron":"\u011B","Ecirc":"\u00CA","ecirc":"\u00EA","ecir":"\u2256","ecolon":"\u2255","Ecy":"\u042D","ecy":"\u044D","eDDot":"\u2A77","Edot":"\u0116","edot":"\u0117","eDot":"\u2251","ee":"\u2147","efDot":"\u2252","Efr":"\uD835\uDD08","efr":"\uD835\uDD22","eg":"\u2A9A","Egrave":"\u00C8","egrave":"\u00E8","egs":"\u2A96","egsdot":"\u2A98","el":"\u2A99","Element":"\u2208","elinters":"\u23E7","ell":"\u2113","els":"\u2A95","elsdot":"\u2A97","Emacr":"\u0112","emacr":"\u0113","empty":"\u2205","emptyset":"\u2205","EmptySmallSquare":"\u25FB","emptyv":"\u2205","EmptyVerySmallSquare":"\u25AB","emsp13":"\u2004","emsp14":"\u2005","emsp":"\u2003","ENG":"\u014A","eng":"\u014B","ensp":"\u2002","Eogon":"\u0118","eogon":"\u0119","Eopf":"\uD835\uDD3C","eopf":"\uD835\uDD56","epar":"\u22D5","eparsl":"\u29E3","eplus":"\u2A71","epsi":"\u03B5","Epsilon":"\u0395","epsilon":"\u03B5","epsiv":"\u03F5","eqcirc":"\u2256","eqcolon":"\u2255","eqsim":"\u2242","eqslantgtr":"\u2A96","eqslantless":"\u2A95","Equal":"\u2A75","equals":"=","EqualTilde":"\u2242","equest":"\u225F","Equilibrium":"\u21CC","equiv":"\u2261","equivDD":"\u2A78","eqvparsl":"\u29E5","erarr":"\u2971","erDot":"\u2253","escr":"\u212F","Escr":"\u2130","esdot":"\u2250","Esim":"\u2A73","esim":"\u2242","Eta":"\u0397","eta":"\u03B7","ETH":"\u00D0","eth":"\u00F0","Euml":"\u00CB","euml":"\u00EB","euro":"\u20AC","excl":"!","exist":"\u2203","Exists":"\u2203","expectation":"\u2130","exponentiale":"\u2147","ExponentialE":"\u2147","fallingdotseq":"\u2252","Fcy":"\u0424","fcy":"\u0444","female":"\u2640","ffilig":"\uFB03","fflig":"\uFB00","ffllig":"\uFB04","Ffr":"\uD835\uDD09","ffr":"\uD835\uDD23","filig":"\uFB01","FilledSmallSquare":"\u25FC","FilledVerySmallSquare":"\u25AA","fjlig":"fj","flat":"\u266D","fllig":"\uFB02","fltns":"\u25B1","fnof":"\u0192","Fopf":"\uD835\uDD3D","fopf":"\uD835\uDD57","forall":"\u2200","ForAll":"\u2200","fork":"\u22D4","forkv":"\u2AD9","Fouriertrf":"\u2131","fpartint":"\u2A0D","frac12":"\u00BD","frac13":"\u2153","frac14":"\u00BC","frac15":"\u2155","frac16":"\u2159","frac18":"\u215B","frac23":"\u2154","frac25":"\u2156","frac34":"\u00BE","frac35":"\u2157","frac38":"\u215C","frac45":"\u2158","frac56":"\u215A","frac58":"\u215D","frac78":"\u215E","frasl":"\u2044","frown":"\u2322","fscr":"\uD835\uDCBB","Fscr":"\u2131","gacute":"\u01F5","Gamma":"\u0393","gamma":"\u03B3","Gammad":"\u03DC","gammad":"\u03DD","gap":"\u2A86","Gbreve":"\u011E","gbreve":"\u011F","Gcedil":"\u0122","Gcirc":"\u011C","gcirc":"\u011D","Gcy":"\u0413","gcy":"\u0433","Gdot":"\u0120","gdot":"\u0121","ge":"\u2265","gE":"\u2267","gEl":"\u2A8C","gel":"\u22DB","geq":"\u2265","geqq":"\u2267","geqslant":"\u2A7E","gescc":"\u2AA9","ges":"\u2A7E","gesdot":"\u2A80","gesdoto":"\u2A82","gesdotol":"\u2A84","gesl":"\u22DB\uFE00","gesles":"\u2A94","Gfr":"\uD835\uDD0A","gfr":"\uD835\uDD24","gg":"\u226B","Gg":"\u22D9","ggg":"\u22D9","gimel":"\u2137","GJcy":"\u0403","gjcy":"\u0453","gla":"\u2AA5","gl":"\u2277","glE":"\u2A92","glj":"\u2AA4","gnap":"\u2A8A","gnapprox":"\u2A8A","gne":"\u2A88","gnE":"\u2269","gneq":"\u2A88","gneqq":"\u2269","gnsim":"\u22E7","Gopf":"\uD835\uDD3E","gopf":"\uD835\uDD58","grave":"`","GreaterEqual":"\u2265","GreaterEqualLess":"\u22DB","GreaterFullEqual":"\u2267","GreaterGreater":"\u2AA2","GreaterLess":"\u2277","GreaterSlantEqual":"\u2A7E","GreaterTilde":"\u2273","Gscr":"\uD835\uDCA2","gscr":"\u210A","gsim":"\u2273","gsime":"\u2A8E","gsiml":"\u2A90","gtcc":"\u2AA7","gtcir":"\u2A7A","gt":">","GT":">","Gt":"\u226B","gtdot":"\u22D7","gtlPar":"\u2995","gtquest":"\u2A7C","gtrapprox":"\u2A86","gtrarr":"\u2978","gtrdot":"\u22D7","gtreqless":"\u22DB","gtreqqless":"\u2A8C","gtrless":"\u2277","gtrsim":"\u2273","gvertneqq":"\u2269\uFE00","gvnE":"\u2269\uFE00","Hacek":"\u02C7","hairsp":"\u200A","half":"\u00BD","hamilt":"\u210B","HARDcy":"\u042A","hardcy":"\u044A","harrcir":"\u2948","harr":"\u2194","hArr":"\u21D4","harrw":"\u21AD","Hat":"^","hbar":"\u210F","Hcirc":"\u0124","hcirc":"\u0125","hearts":"\u2665","heartsuit":"\u2665","hellip":"\u2026","hercon":"\u22B9","hfr":"\uD835\uDD25","Hfr":"\u210C","HilbertSpace":"\u210B","hksearow":"\u2925","hkswarow":"\u2926","hoarr":"\u21FF","homtht":"\u223B","hookleftarrow":"\u21A9","hookrightarrow":"\u21AA","hopf":"\uD835\uDD59","Hopf":"\u210D","horbar":"\u2015","HorizontalLine":"\u2500","hscr":"\uD835\uDCBD","Hscr":"\u210B","hslash":"\u210F","Hstrok":"\u0126","hstrok":"\u0127","HumpDownHump":"\u224E","HumpEqual":"\u224F","hybull":"\u2043","hyphen":"\u2010","Iacute":"\u00CD","iacute":"\u00ED","ic":"\u2063","Icirc":"\u00CE","icirc":"\u00EE","Icy":"\u0418","icy":"\u0438","Idot":"\u0130","IEcy":"\u0415","iecy":"\u0435","iexcl":"\u00A1","iff":"\u21D4","ifr":"\uD835\uDD26","Ifr":"\u2111","Igrave":"\u00CC","igrave":"\u00EC","ii":"\u2148","iiiint":"\u2A0C","iiint":"\u222D","iinfin":"\u29DC","iiota":"\u2129","IJlig":"\u0132","ijlig":"\u0133","Imacr":"\u012A","imacr":"\u012B","image":"\u2111","ImaginaryI":"\u2148","imagline":"\u2110","imagpart":"\u2111","imath":"\u0131","Im":"\u2111","imof":"\u22B7","imped":"\u01B5","Implies":"\u21D2","incare":"\u2105","in":"\u2208","infin":"\u221E","infintie":"\u29DD","inodot":"\u0131","intcal":"\u22BA","int":"\u222B","Int":"\u222C","integers":"\u2124","Integral":"\u222B","intercal":"\u22BA","Intersection":"\u22C2","intlarhk":"\u2A17","intprod":"\u2A3C","InvisibleComma":"\u2063","InvisibleTimes":"\u2062","IOcy":"\u0401","iocy":"\u0451","Iogon":"\u012E","iogon":"\u012F","Iopf":"\uD835\uDD40","iopf":"\uD835\uDD5A","Iota":"\u0399","iota":"\u03B9","iprod":"\u2A3C","iquest":"\u00BF","iscr":"\uD835\uDCBE","Iscr":"\u2110","isin":"\u2208","isindot":"\u22F5","isinE":"\u22F9","isins":"\u22F4","isinsv":"\u22F3","isinv":"\u2208","it":"\u2062","Itilde":"\u0128","itilde":"\u0129","Iukcy":"\u0406","iukcy":"\u0456","Iuml":"\u00CF","iuml":"\u00EF","Jcirc":"\u0134","jcirc":"\u0135","Jcy":"\u0419","jcy":"\u0439","Jfr":"\uD835\uDD0D","jfr":"\uD835\uDD27","jmath":"\u0237","Jopf":"\uD835\uDD41","jopf":"\uD835\uDD5B","Jscr":"\uD835\uDCA5","jscr":"\uD835\uDCBF","Jsercy":"\u0408","jsercy":"\u0458","Jukcy":"\u0404","jukcy":"\u0454","Kappa":"\u039A","kappa":"\u03BA","kappav":"\u03F0","Kcedil":"\u0136","kcedil":"\u0137","Kcy":"\u041A","kcy":"\u043A","Kfr":"\uD835\uDD0E","kfr":"\uD835\uDD28","kgreen":"\u0138","KHcy":"\u0425","khcy":"\u0445","KJcy":"\u040C","kjcy":"\u045C","Kopf":"\uD835\uDD42","kopf":"\uD835\uDD5C","Kscr":"\uD835\uDCA6","kscr":"\uD835\uDCC0","lAarr":"\u21DA","Lacute":"\u0139","lacute":"\u013A","laemptyv":"\u29B4","lagran":"\u2112","Lambda":"\u039B","lambda":"\u03BB","lang":"\u27E8","Lang":"\u27EA","langd":"\u2991","langle":"\u27E8","lap":"\u2A85","Laplacetrf":"\u2112","laquo":"\u00AB","larrb":"\u21E4","larrbfs":"\u291F","larr":"\u2190","Larr":"\u219E","lArr":"\u21D0","larrfs":"\u291D","larrhk":"\u21A9","larrlp":"\u21AB","larrpl":"\u2939","larrsim":"\u2973","larrtl":"\u21A2","latail":"\u2919","lAtail":"\u291B","lat":"\u2AAB","late":"\u2AAD","lates":"\u2AAD\uFE00","lbarr":"\u290C","lBarr":"\u290E","lbbrk":"\u2772","lbrace":"{","lbrack":"[","lbrke":"\u298B","lbrksld":"\u298F","lbrkslu":"\u298D","Lcaron":"\u013D","lcaron":"\u013E","Lcedil":"\u013B","lcedil":"\u013C","lceil":"\u2308","lcub":"{","Lcy":"\u041B","lcy":"\u043B","ldca":"\u2936","ldquo":"\u201C","ldquor":"\u201E","ldrdhar":"\u2967","ldrushar":"\u294B","ldsh":"\u21B2","le":"\u2264","lE":"\u2266","LeftAngleBracket":"\u27E8","LeftArrowBar":"\u21E4","leftarrow":"\u2190","LeftArrow":"\u2190","Leftarrow":"\u21D0","LeftArrowRightArrow":"\u21C6","leftarrowtail":"\u21A2","LeftCeiling":"\u2308","LeftDoubleBracket":"\u27E6","LeftDownTeeVector":"\u2961","LeftDownVectorBar":"\u2959","LeftDownVector":"\u21C3","LeftFloor":"\u230A","leftharpoondown":"\u21BD","leftharpoonup":"\u21BC","leftleftarrows":"\u21C7","leftrightarrow":"\u2194","LeftRightArrow":"\u2194","Leftrightarrow":"\u21D4","leftrightarrows":"\u21C6","leftrightharpoons":"\u21CB","leftrightsquigarrow":"\u21AD","LeftRightVector":"\u294E","LeftTeeArrow":"\u21A4","LeftTee":"\u22A3","LeftTeeVector":"\u295A","leftthreetimes":"\u22CB","LeftTriangleBar":"\u29CF","LeftTriangle":"\u22B2","LeftTriangleEqual":"\u22B4","LeftUpDownVector":"\u2951","LeftUpTeeVector":"\u2960","LeftUpVectorBar":"\u2958","LeftUpVector":"\u21BF","LeftVectorBar":"\u2952","LeftVector":"\u21BC","lEg":"\u2A8B","leg":"\u22DA","leq":"\u2264","leqq":"\u2266","leqslant":"\u2A7D","lescc":"\u2AA8","les":"\u2A7D","lesdot":"\u2A7F","lesdoto":"\u2A81","lesdotor":"\u2A83","lesg":"\u22DA\uFE00","lesges":"\u2A93","lessapprox":"\u2A85","lessdot":"\u22D6","lesseqgtr":"\u22DA","lesseqqgtr":"\u2A8B","LessEqualGreater":"\u22DA","LessFullEqual":"\u2266","LessGreater":"\u2276","lessgtr":"\u2276","LessLess":"\u2AA1","lesssim":"\u2272","LessSlantEqual":"\u2A7D","LessTilde":"\u2272","lfisht":"\u297C","lfloor":"\u230A","Lfr":"\uD835\uDD0F","lfr":"\uD835\uDD29","lg":"\u2276","lgE":"\u2A91","lHar":"\u2962","lhard":"\u21BD","lharu":"\u21BC","lharul":"\u296A","lhblk":"\u2584","LJcy":"\u0409","ljcy":"\u0459","llarr":"\u21C7","ll":"\u226A","Ll":"\u22D8","llcorner":"\u231E","Lleftarrow":"\u21DA","llhard":"\u296B","lltri":"\u25FA","Lmidot":"\u013F","lmidot":"\u0140","lmoustache":"\u23B0","lmoust":"\u23B0","lnap":"\u2A89","lnapprox":"\u2A89","lne":"\u2A87","lnE":"\u2268","lneq":"\u2A87","lneqq":"\u2268","lnsim":"\u22E6","loang":"\u27EC","loarr":"\u21FD","lobrk":"\u27E6","longleftarrow":"\u27F5","LongLeftArrow":"\u27F5","Longleftarrow":"\u27F8","longleftrightarrow":"\u27F7","LongLeftRightArrow":"\u27F7","Longleftrightarrow":"\u27FA","longmapsto":"\u27FC","longrightarrow":"\u27F6","LongRightArrow":"\u27F6","Longrightarrow":"\u27F9","looparrowleft":"\u21AB","looparrowright":"\u21AC","lopar":"\u2985","Lopf":"\uD835\uDD43","lopf":"\uD835\uDD5D","loplus":"\u2A2D","lotimes":"\u2A34","lowast":"\u2217","lowbar":"_","LowerLeftArrow":"\u2199","LowerRightArrow":"\u2198","loz":"\u25CA","lozenge":"\u25CA","lozf":"\u29EB","lpar":"(","lparlt":"\u2993","lrarr":"\u21C6","lrcorner":"\u231F","lrhar":"\u21CB","lrhard":"\u296D","lrm":"\u200E","lrtri":"\u22BF","lsaquo":"\u2039","lscr":"\uD835\uDCC1","Lscr":"\u2112","lsh":"\u21B0","Lsh":"\u21B0","lsim":"\u2272","lsime":"\u2A8D","lsimg":"\u2A8F","lsqb":"[","lsquo":"\u2018","lsquor":"\u201A","Lstrok":"\u0141","lstrok":"\u0142","ltcc":"\u2AA6","ltcir":"\u2A79","lt":"<","LT":"<","Lt":"\u226A","ltdot":"\u22D6","lthree":"\u22CB","ltimes":"\u22C9","ltlarr":"\u2976","ltquest":"\u2A7B","ltri":"\u25C3","ltrie":"\u22B4","ltrif":"\u25C2","ltrPar":"\u2996","lurdshar":"\u294A","luruhar":"\u2966","lvertneqq":"\u2268\uFE00","lvnE":"\u2268\uFE00","macr":"\u00AF","male":"\u2642","malt":"\u2720","maltese":"\u2720","Map":"\u2905","map":"\u21A6","mapsto":"\u21A6","mapstodown":"\u21A7","mapstoleft":"\u21A4","mapstoup":"\u21A5","marker":"\u25AE","mcomma":"\u2A29","Mcy":"\u041C","mcy":"\u043C","mdash":"\u2014","mDDot":"\u223A","measuredangle":"\u2221","MediumSpace":"\u205F","Mellintrf":"\u2133","Mfr":"\uD835\uDD10","mfr":"\uD835\uDD2A","mho":"\u2127","micro":"\u00B5","midast":"*","midcir":"\u2AF0","mid":"\u2223","middot":"\u00B7","minusb":"\u229F","minus":"\u2212","minusd":"\u2238","minusdu":"\u2A2A","MinusPlus":"\u2213","mlcp":"\u2ADB","mldr":"\u2026","mnplus":"\u2213","models":"\u22A7","Mopf":"\uD835\uDD44","mopf":"\uD835\uDD5E","mp":"\u2213","mscr":"\uD835\uDCC2","Mscr":"\u2133","mstpos":"\u223E","Mu":"\u039C","mu":"\u03BC","multimap":"\u22B8","mumap":"\u22B8","nabla":"\u2207","Nacute":"\u0143","nacute":"\u0144","nang":"\u2220\u20D2","nap":"\u2249","napE":"\u2A70\u0338","napid":"\u224B\u0338","napos":"\u0149","napprox":"\u2249","natural":"\u266E","naturals":"\u2115","natur":"\u266E","nbsp":"\u00A0","nbump":"\u224E\u0338","nbumpe":"\u224F\u0338","ncap":"\u2A43","Ncaron":"\u0147","ncaron":"\u0148","Ncedil":"\u0145","ncedil":"\u0146","ncong":"\u2247","ncongdot":"\u2A6D\u0338","ncup":"\u2A42","Ncy":"\u041D","ncy":"\u043D","ndash":"\u2013","nearhk":"\u2924","nearr":"\u2197","neArr":"\u21D7","nearrow":"\u2197","ne":"\u2260","nedot":"\u2250\u0338","NegativeMediumSpace":"\u200B","NegativeThickSpace":"\u200B","NegativeThinSpace":"\u200B","NegativeVeryThinSpace":"\u200B","nequiv":"\u2262","nesear":"\u2928","nesim":"\u2242\u0338","NestedGreaterGreater":"\u226B","NestedLessLess":"\u226A","NewLine":"\n","nexist":"\u2204","nexists":"\u2204","Nfr":"\uD835\uDD11","nfr":"\uD835\uDD2B","ngE":"\u2267\u0338","nge":"\u2271","ngeq":"\u2271","ngeqq":"\u2267\u0338","ngeqslant":"\u2A7E\u0338","nges":"\u2A7E\u0338","nGg":"\u22D9\u0338","ngsim":"\u2275","nGt":"\u226B\u20D2","ngt":"\u226F","ngtr":"\u226F","nGtv":"\u226B\u0338","nharr":"\u21AE","nhArr":"\u21CE","nhpar":"\u2AF2","ni":"\u220B","nis":"\u22FC","nisd":"\u22FA","niv":"\u220B","NJcy":"\u040A","njcy":"\u045A","nlarr":"\u219A","nlArr":"\u21CD","nldr":"\u2025","nlE":"\u2266\u0338","nle":"\u2270","nleftarrow":"\u219A","nLeftarrow":"\u21CD","nleftrightarrow":"\u21AE","nLeftrightarrow":"\u21CE","nleq":"\u2270","nleqq":"\u2266\u0338","nleqslant":"\u2A7D\u0338","nles":"\u2A7D\u0338","nless":"\u226E","nLl":"\u22D8\u0338","nlsim":"\u2274","nLt":"\u226A\u20D2","nlt":"\u226E","nltri":"\u22EA","nltrie":"\u22EC","nLtv":"\u226A\u0338","nmid":"\u2224","NoBreak":"\u2060","NonBreakingSpace":"\u00A0","nopf":"\uD835\uDD5F","Nopf":"\u2115","Not":"\u2AEC","not":"\u00AC","NotCongruent":"\u2262","NotCupCap":"\u226D","NotDoubleVerticalBar":"\u2226","NotElement":"\u2209","NotEqual":"\u2260","NotEqualTilde":"\u2242\u0338","NotExists":"\u2204","NotGreater":"\u226F","NotGreaterEqual":"\u2271","NotGreaterFullEqual":"\u2267\u0338","NotGreaterGreater":"\u226B\u0338","NotGreaterLess":"\u2279","NotGreaterSlantEqual":"\u2A7E\u0338","NotGreaterTilde":"\u2275","NotHumpDownHump":"\u224E\u0338","NotHumpEqual":"\u224F\u0338","notin":"\u2209","notindot":"\u22F5\u0338","notinE":"\u22F9\u0338","notinva":"\u2209","notinvb":"\u22F7","notinvc":"\u22F6","NotLeftTriangleBar":"\u29CF\u0338","NotLeftTriangle":"\u22EA","NotLeftTriangleEqual":"\u22EC","NotLess":"\u226E","NotLessEqual":"\u2270","NotLessGreater":"\u2278","NotLessLess":"\u226A\u0338","NotLessSlantEqual":"\u2A7D\u0338","NotLessTilde":"\u2274","NotNestedGreaterGreater":"\u2AA2\u0338","NotNestedLessLess":"\u2AA1\u0338","notni":"\u220C","notniva":"\u220C","notnivb":"\u22FE","notnivc":"\u22FD","NotPrecedes":"\u2280","NotPrecedesEqual":"\u2AAF\u0338","NotPrecedesSlantEqual":"\u22E0","NotReverseElement":"\u220C","NotRightTriangleBar":"\u29D0\u0338","NotRightTriangle":"\u22EB","NotRightTriangleEqual":"\u22ED","NotSquareSubset":"\u228F\u0338","NotSquareSubsetEqual":"\u22E2","NotSquareSuperset":"\u2290\u0338","NotSquareSupersetEqual":"\u22E3","NotSubset":"\u2282\u20D2","NotSubsetEqual":"\u2288","NotSucceeds":"\u2281","NotSucceedsEqual":"\u2AB0\u0338","NotSucceedsSlantEqual":"\u22E1","NotSucceedsTilde":"\u227F\u0338","NotSuperset":"\u2283\u20D2","NotSupersetEqual":"\u2289","NotTilde":"\u2241","NotTildeEqual":"\u2244","NotTildeFullEqual":"\u2247","NotTildeTilde":"\u2249","NotVerticalBar":"\u2224","nparallel":"\u2226","npar":"\u2226","nparsl":"\u2AFD\u20E5","npart":"\u2202\u0338","npolint":"\u2A14","npr":"\u2280","nprcue":"\u22E0","nprec":"\u2280","npreceq":"\u2AAF\u0338","npre":"\u2AAF\u0338","nrarrc":"\u2933\u0338","nrarr":"\u219B","nrArr":"\u21CF","nrarrw":"\u219D\u0338","nrightarrow":"\u219B","nRightarrow":"\u21CF","nrtri":"\u22EB","nrtrie":"\u22ED","nsc":"\u2281","nsccue":"\u22E1","nsce":"\u2AB0\u0338","Nscr":"\uD835\uDCA9","nscr":"\uD835\uDCC3","nshortmid":"\u2224","nshortparallel":"\u2226","nsim":"\u2241","nsime":"\u2244","nsimeq":"\u2244","nsmid":"\u2224","nspar":"\u2226","nsqsube":"\u22E2","nsqsupe":"\u22E3","nsub":"\u2284","nsubE":"\u2AC5\u0338","nsube":"\u2288","nsubset":"\u2282\u20D2","nsubseteq":"\u2288","nsubseteqq":"\u2AC5\u0338","nsucc":"\u2281","nsucceq":"\u2AB0\u0338","nsup":"\u2285","nsupE":"\u2AC6\u0338","nsupe":"\u2289","nsupset":"\u2283\u20D2","nsupseteq":"\u2289","nsupseteqq":"\u2AC6\u0338","ntgl":"\u2279","Ntilde":"\u00D1","ntilde":"\u00F1","ntlg":"\u2278","ntriangleleft":"\u22EA","ntrianglelefteq":"\u22EC","ntriangleright":"\u22EB","ntrianglerighteq":"\u22ED","Nu":"\u039D","nu":"\u03BD","num":"#","numero":"\u2116","numsp":"\u2007","nvap":"\u224D\u20D2","nvdash":"\u22AC","nvDash":"\u22AD","nVdash":"\u22AE","nVDash":"\u22AF","nvge":"\u2265\u20D2","nvgt":">\u20D2","nvHarr":"\u2904","nvinfin":"\u29DE","nvlArr":"\u2902","nvle":"\u2264\u20D2","nvlt":"<\u20D2","nvltrie":"\u22B4\u20D2","nvrArr":"\u2903","nvrtrie":"\u22B5\u20D2","nvsim":"\u223C\u20D2","nwarhk":"\u2923","nwarr":"\u2196","nwArr":"\u21D6","nwarrow":"\u2196","nwnear":"\u2927","Oacute":"\u00D3","oacute":"\u00F3","oast":"\u229B","Ocirc":"\u00D4","ocirc":"\u00F4","ocir":"\u229A","Ocy":"\u041E","ocy":"\u043E","odash":"\u229D","Odblac":"\u0150","odblac":"\u0151","odiv":"\u2A38","odot":"\u2299","odsold":"\u29BC","OElig":"\u0152","oelig":"\u0153","ofcir":"\u29BF","Ofr":"\uD835\uDD12","ofr":"\uD835\uDD2C","ogon":"\u02DB","Ograve":"\u00D2","ograve":"\u00F2","ogt":"\u29C1","ohbar":"\u29B5","ohm":"\u03A9","oint":"\u222E","olarr":"\u21BA","olcir":"\u29BE","olcross":"\u29BB","oline":"\u203E","olt":"\u29C0","Omacr":"\u014C","omacr":"\u014D","Omega":"\u03A9","omega":"\u03C9","Omicron":"\u039F","omicron":"\u03BF","omid":"\u29B6","ominus":"\u2296","Oopf":"\uD835\uDD46","oopf":"\uD835\uDD60","opar":"\u29B7","OpenCurlyDoubleQuote":"\u201C","OpenCurlyQuote":"\u2018","operp":"\u29B9","oplus":"\u2295","orarr":"\u21BB","Or":"\u2A54","or":"\u2228","ord":"\u2A5D","order":"\u2134","orderof":"\u2134","ordf":"\u00AA","ordm":"\u00BA","origof":"\u22B6","oror":"\u2A56","orslope":"\u2A57","orv":"\u2A5B","oS":"\u24C8","Oscr":"\uD835\uDCAA","oscr":"\u2134","Oslash":"\u00D8","oslash":"\u00F8","osol":"\u2298","Otilde":"\u00D5","otilde":"\u00F5","otimesas":"\u2A36","Otimes":"\u2A37","otimes":"\u2297","Ouml":"\u00D6","ouml":"\u00F6","ovbar":"\u233D","OverBar":"\u203E","OverBrace":"\u23DE","OverBracket":"\u23B4","OverParenthesis":"\u23DC","para":"\u00B6","parallel":"\u2225","par":"\u2225","parsim":"\u2AF3","parsl":"\u2AFD","part":"\u2202","PartialD":"\u2202","Pcy":"\u041F","pcy":"\u043F","percnt":"%","period":".","permil":"\u2030","perp":"\u22A5","pertenk":"\u2031","Pfr":"\uD835\uDD13","pfr":"\uD835\uDD2D","Phi":"\u03A6","phi":"\u03C6","phiv":"\u03D5","phmmat":"\u2133","phone":"\u260E","Pi":"\u03A0","pi":"\u03C0","pitchfork":"\u22D4","piv":"\u03D6","planck":"\u210F","planckh":"\u210E","plankv":"\u210F","plusacir":"\u2A23","plusb":"\u229E","pluscir":"\u2A22","plus":"+","plusdo":"\u2214","plusdu":"\u2A25","pluse":"\u2A72","PlusMinus":"\u00B1","plusmn":"\u00B1","plussim":"\u2A26","plustwo":"\u2A27","pm":"\u00B1","Poincareplane":"\u210C","pointint":"\u2A15","popf":"\uD835\uDD61","Popf":"\u2119","pound":"\u00A3","prap":"\u2AB7","Pr":"\u2ABB","pr":"\u227A","prcue":"\u227C","precapprox":"\u2AB7","prec":"\u227A","preccurlyeq":"\u227C","Precedes":"\u227A","PrecedesEqual":"\u2AAF","PrecedesSlantEqual":"\u227C","PrecedesTilde":"\u227E","preceq":"\u2AAF","precnapprox":"\u2AB9","precneqq":"\u2AB5","precnsim":"\u22E8","pre":"\u2AAF","prE":"\u2AB3","precsim":"\u227E","prime":"\u2032","Prime":"\u2033","primes":"\u2119","prnap":"\u2AB9","prnE":"\u2AB5","prnsim":"\u22E8","prod":"\u220F","Product":"\u220F","profalar":"\u232E","profline":"\u2312","profsurf":"\u2313","prop":"\u221D","Proportional":"\u221D","Proportion":"\u2237","propto":"\u221D","prsim":"\u227E","prurel":"\u22B0","Pscr":"\uD835\uDCAB","pscr":"\uD835\uDCC5","Psi":"\u03A8","psi":"\u03C8","puncsp":"\u2008","Qfr":"\uD835\uDD14","qfr":"\uD835\uDD2E","qint":"\u2A0C","qopf":"\uD835\uDD62","Qopf":"\u211A","qprime":"\u2057","Qscr":"\uD835\uDCAC","qscr":"\uD835\uDCC6","quaternions":"\u210D","quatint":"\u2A16","quest":"?","questeq":"\u225F","quot":"\"","QUOT":"\"","rAarr":"\u21DB","race":"\u223D\u0331","Racute":"\u0154","racute":"\u0155","radic":"\u221A","raemptyv":"\u29B3","rang":"\u27E9","Rang":"\u27EB","rangd":"\u2992","range":"\u29A5","rangle":"\u27E9","raquo":"\u00BB","rarrap":"\u2975","rarrb":"\u21E5","rarrbfs":"\u2920","rarrc":"\u2933","rarr":"\u2192","Rarr":"\u21A0","rArr":"\u21D2","rarrfs":"\u291E","rarrhk":"\u21AA","rarrlp":"\u21AC","rarrpl":"\u2945","rarrsim":"\u2974","Rarrtl":"\u2916","rarrtl":"\u21A3","rarrw":"\u219D","ratail":"\u291A","rAtail":"\u291C","ratio":"\u2236","rationals":"\u211A","rbarr":"\u290D","rBarr":"\u290F","RBarr":"\u2910","rbbrk":"\u2773","rbrace":"}","rbrack":"]","rbrke":"\u298C","rbrksld":"\u298E","rbrkslu":"\u2990","Rcaron":"\u0158","rcaron":"\u0159","Rcedil":"\u0156","rcedil":"\u0157","rceil":"\u2309","rcub":"}","Rcy":"\u0420","rcy":"\u0440","rdca":"\u2937","rdldhar":"\u2969","rdquo":"\u201D","rdquor":"\u201D","rdsh":"\u21B3","real":"\u211C","realine":"\u211B","realpart":"\u211C","reals":"\u211D","Re":"\u211C","rect":"\u25AD","reg":"\u00AE","REG":"\u00AE","ReverseElement":"\u220B","ReverseEquilibrium":"\u21CB","ReverseUpEquilibrium":"\u296F","rfisht":"\u297D","rfloor":"\u230B","rfr":"\uD835\uDD2F","Rfr":"\u211C","rHar":"\u2964","rhard":"\u21C1","rharu":"\u21C0","rharul":"\u296C","Rho":"\u03A1","rho":"\u03C1","rhov":"\u03F1","RightAngleBracket":"\u27E9","RightArrowBar":"\u21E5","rightarrow":"\u2192","RightArrow":"\u2192","Rightarrow":"\u21D2","RightArrowLeftArrow":"\u21C4","rightarrowtail":"\u21A3","RightCeiling":"\u2309","RightDoubleBracket":"\u27E7","RightDownTeeVector":"\u295D","RightDownVectorBar":"\u2955","RightDownVector":"\u21C2","RightFloor":"\u230B","rightharpoondown":"\u21C1","rightharpoonup":"\u21C0","rightleftarrows":"\u21C4","rightleftharpoons":"\u21CC","rightrightarrows":"\u21C9","rightsquigarrow":"\u219D","RightTeeArrow":"\u21A6","RightTee":"\u22A2","RightTeeVector":"\u295B","rightthreetimes":"\u22CC","RightTriangleBar":"\u29D0","RightTriangle":"\u22B3","RightTriangleEqual":"\u22B5","RightUpDownVector":"\u294F","RightUpTeeVector":"\u295C","RightUpVectorBar":"\u2954","RightUpVector":"\u21BE","RightVectorBar":"\u2953","RightVector":"\u21C0","ring":"\u02DA","risingdotseq":"\u2253","rlarr":"\u21C4","rlhar":"\u21CC","rlm":"\u200F","rmoustache":"\u23B1","rmoust":"\u23B1","rnmid":"\u2AEE","roang":"\u27ED","roarr":"\u21FE","robrk":"\u27E7","ropar":"\u2986","ropf":"\uD835\uDD63","Ropf":"\u211D","roplus":"\u2A2E","rotimes":"\u2A35","RoundImplies":"\u2970","rpar":")","rpargt":"\u2994","rppolint":"\u2A12","rrarr":"\u21C9","Rrightarrow":"\u21DB","rsaquo":"\u203A","rscr":"\uD835\uDCC7","Rscr":"\u211B","rsh":"\u21B1","Rsh":"\u21B1","rsqb":"]","rsquo":"\u2019","rsquor":"\u2019","rthree":"\u22CC","rtimes":"\u22CA","rtri":"\u25B9","rtrie":"\u22B5","rtrif":"\u25B8","rtriltri":"\u29CE","RuleDelayed":"\u29F4","ruluhar":"\u2968","rx":"\u211E","Sacute":"\u015A","sacute":"\u015B","sbquo":"\u201A","scap":"\u2AB8","Scaron":"\u0160","scaron":"\u0161","Sc":"\u2ABC","sc":"\u227B","sccue":"\u227D","sce":"\u2AB0","scE":"\u2AB4","Scedil":"\u015E","scedil":"\u015F","Scirc":"\u015C","scirc":"\u015D","scnap":"\u2ABA","scnE":"\u2AB6","scnsim":"\u22E9","scpolint":"\u2A13","scsim":"\u227F","Scy":"\u0421","scy":"\u0441","sdotb":"\u22A1","sdot":"\u22C5","sdote":"\u2A66","searhk":"\u2925","searr":"\u2198","seArr":"\u21D8","searrow":"\u2198","sect":"\u00A7","semi":";","seswar":"\u2929","setminus":"\u2216","setmn":"\u2216","sext":"\u2736","Sfr":"\uD835\uDD16","sfr":"\uD835\uDD30","sfrown":"\u2322","sharp":"\u266F","SHCHcy":"\u0429","shchcy":"\u0449","SHcy":"\u0428","shcy":"\u0448","ShortDownArrow":"\u2193","ShortLeftArrow":"\u2190","shortmid":"\u2223","shortparallel":"\u2225","ShortRightArrow":"\u2192","ShortUpArrow":"\u2191","shy":"\u00AD","Sigma":"\u03A3","sigma":"\u03C3","sigmaf":"\u03C2","sigmav":"\u03C2","sim":"\u223C","simdot":"\u2A6A","sime":"\u2243","simeq":"\u2243","simg":"\u2A9E","simgE":"\u2AA0","siml":"\u2A9D","simlE":"\u2A9F","simne":"\u2246","simplus":"\u2A24","simrarr":"\u2972","slarr":"\u2190","SmallCircle":"\u2218","smallsetminus":"\u2216","smashp":"\u2A33","smeparsl":"\u29E4","smid":"\u2223","smile":"\u2323","smt":"\u2AAA","smte":"\u2AAC","smtes":"\u2AAC\uFE00","SOFTcy":"\u042C","softcy":"\u044C","solbar":"\u233F","solb":"\u29C4","sol":"/","Sopf":"\uD835\uDD4A","sopf":"\uD835\uDD64","spades":"\u2660","spadesuit":"\u2660","spar":"\u2225","sqcap":"\u2293","sqcaps":"\u2293\uFE00","sqcup":"\u2294","sqcups":"\u2294\uFE00","Sqrt":"\u221A","sqsub":"\u228F","sqsube":"\u2291","sqsubset":"\u228F","sqsubseteq":"\u2291","sqsup":"\u2290","sqsupe":"\u2292","sqsupset":"\u2290","sqsupseteq":"\u2292","square":"\u25A1","Square":"\u25A1","SquareIntersection":"\u2293","SquareSubset":"\u228F","SquareSubsetEqual":"\u2291","SquareSuperset":"\u2290","SquareSupersetEqual":"\u2292","SquareUnion":"\u2294","squarf":"\u25AA","squ":"\u25A1","squf":"\u25AA","srarr":"\u2192","Sscr":"\uD835\uDCAE","sscr":"\uD835\uDCC8","ssetmn":"\u2216","ssmile":"\u2323","sstarf":"\u22C6","Star":"\u22C6","star":"\u2606","starf":"\u2605","straightepsilon":"\u03F5","straightphi":"\u03D5","strns":"\u00AF","sub":"\u2282","Sub":"\u22D0","subdot":"\u2ABD","subE":"\u2AC5","sube":"\u2286","subedot":"\u2AC3","submult":"\u2AC1","subnE":"\u2ACB","subne":"\u228A","subplus":"\u2ABF","subrarr":"\u2979","subset":"\u2282","Subset":"\u22D0","subseteq":"\u2286","subseteqq":"\u2AC5","SubsetEqual":"\u2286","subsetneq":"\u228A","subsetneqq":"\u2ACB","subsim":"\u2AC7","subsub":"\u2AD5","subsup":"\u2AD3","succapprox":"\u2AB8","succ":"\u227B","succcurlyeq":"\u227D","Succeeds":"\u227B","SucceedsEqual":"\u2AB0","SucceedsSlantEqual":"\u227D","SucceedsTilde":"\u227F","succeq":"\u2AB0","succnapprox":"\u2ABA","succneqq":"\u2AB6","succnsim":"\u22E9","succsim":"\u227F","SuchThat":"\u220B","sum":"\u2211","Sum":"\u2211","sung":"\u266A","sup1":"\u00B9","sup2":"\u00B2","sup3":"\u00B3","sup":"\u2283","Sup":"\u22D1","supdot":"\u2ABE","supdsub":"\u2AD8","supE":"\u2AC6","supe":"\u2287","supedot":"\u2AC4","Superset":"\u2283","SupersetEqual":"\u2287","suphsol":"\u27C9","suphsub":"\u2AD7","suplarr":"\u297B","supmult":"\u2AC2","supnE":"\u2ACC","supne":"\u228B","supplus":"\u2AC0","supset":"\u2283","Supset":"\u22D1","supseteq":"\u2287","supseteqq":"\u2AC6","supsetneq":"\u228B","supsetneqq":"\u2ACC","supsim":"\u2AC8","supsub":"\u2AD4","supsup":"\u2AD6","swarhk":"\u2926","swarr":"\u2199","swArr":"\u21D9","swarrow":"\u2199","swnwar":"\u292A","szlig":"\u00DF","Tab":"\t","target":"\u2316","Tau":"\u03A4","tau":"\u03C4","tbrk":"\u23B4","Tcaron":"\u0164","tcaron":"\u0165","Tcedil":"\u0162","tcedil":"\u0163","Tcy":"\u0422","tcy":"\u0442","tdot":"\u20DB","telrec":"\u2315","Tfr":"\uD835\uDD17","tfr":"\uD835\uDD31","there4":"\u2234","therefore":"\u2234","Therefore":"\u2234","Theta":"\u0398","theta":"\u03B8","thetasym":"\u03D1","thetav":"\u03D1","thickapprox":"\u2248","thicksim":"\u223C","ThickSpace":"\u205F\u200A","ThinSpace":"\u2009","thinsp":"\u2009","thkap":"\u2248","thksim":"\u223C","THORN":"\u00DE","thorn":"\u00FE","tilde":"\u02DC","Tilde":"\u223C","TildeEqual":"\u2243","TildeFullEqual":"\u2245","TildeTilde":"\u2248","timesbar":"\u2A31","timesb":"\u22A0","times":"\u00D7","timesd":"\u2A30","tint":"\u222D","toea":"\u2928","topbot":"\u2336","topcir":"\u2AF1","top":"\u22A4","Topf":"\uD835\uDD4B","topf":"\uD835\uDD65","topfork":"\u2ADA","tosa":"\u2929","tprime":"\u2034","trade":"\u2122","TRADE":"\u2122","triangle":"\u25B5","triangledown":"\u25BF","triangleleft":"\u25C3","trianglelefteq":"\u22B4","triangleq":"\u225C","triangleright":"\u25B9","trianglerighteq":"\u22B5","tridot":"\u25EC","trie":"\u225C","triminus":"\u2A3A","TripleDot":"\u20DB","triplus":"\u2A39","trisb":"\u29CD","tritime":"\u2A3B","trpezium":"\u23E2","Tscr":"\uD835\uDCAF","tscr":"\uD835\uDCC9","TScy":"\u0426","tscy":"\u0446","TSHcy":"\u040B","tshcy":"\u045B","Tstrok":"\u0166","tstrok":"\u0167","twixt":"\u226C","twoheadleftarrow":"\u219E","twoheadrightarrow":"\u21A0","Uacute":"\u00DA","uacute":"\u00FA","uarr":"\u2191","Uarr":"\u219F","uArr":"\u21D1","Uarrocir":"\u2949","Ubrcy":"\u040E","ubrcy":"\u045E","Ubreve":"\u016C","ubreve":"\u016D","Ucirc":"\u00DB","ucirc":"\u00FB","Ucy":"\u0423","ucy":"\u0443","udarr":"\u21C5","Udblac":"\u0170","udblac":"\u0171","udhar":"\u296E","ufisht":"\u297E","Ufr":"\uD835\uDD18","ufr":"\uD835\uDD32","Ugrave":"\u00D9","ugrave":"\u00F9","uHar":"\u2963","uharl":"\u21BF","uharr":"\u21BE","uhblk":"\u2580","ulcorn":"\u231C","ulcorner":"\u231C","ulcrop":"\u230F","ultri":"\u25F8","Umacr":"\u016A","umacr":"\u016B","uml":"\u00A8","UnderBar":"_","UnderBrace":"\u23DF","UnderBracket":"\u23B5","UnderParenthesis":"\u23DD","Union":"\u22C3","UnionPlus":"\u228E","Uogon":"\u0172","uogon":"\u0173","Uopf":"\uD835\uDD4C","uopf":"\uD835\uDD66","UpArrowBar":"\u2912","uparrow":"\u2191","UpArrow":"\u2191","Uparrow":"\u21D1","UpArrowDownArrow":"\u21C5","updownarrow":"\u2195","UpDownArrow":"\u2195","Updownarrow":"\u21D5","UpEquilibrium":"\u296E","upharpoonleft":"\u21BF","upharpoonright":"\u21BE","uplus":"\u228E","UpperLeftArrow":"\u2196","UpperRightArrow":"\u2197","upsi":"\u03C5","Upsi":"\u03D2","upsih":"\u03D2","Upsilon":"\u03A5","upsilon":"\u03C5","UpTeeArrow":"\u21A5","UpTee":"\u22A5","upuparrows":"\u21C8","urcorn":"\u231D","urcorner":"\u231D","urcrop":"\u230E","Uring":"\u016E","uring":"\u016F","urtri":"\u25F9","Uscr":"\uD835\uDCB0","uscr":"\uD835\uDCCA","utdot":"\u22F0","Utilde":"\u0168","utilde":"\u0169","utri":"\u25B5","utrif":"\u25B4","uuarr":"\u21C8","Uuml":"\u00DC","uuml":"\u00FC","uwangle":"\u29A7","vangrt":"\u299C","varepsilon":"\u03F5","varkappa":"\u03F0","varnothing":"\u2205","varphi":"\u03D5","varpi":"\u03D6","varpropto":"\u221D","varr":"\u2195","vArr":"\u21D5","varrho":"\u03F1","varsigma":"\u03C2","varsubsetneq":"\u228A\uFE00","varsubsetneqq":"\u2ACB\uFE00","varsupsetneq":"\u228B\uFE00","varsupsetneqq":"\u2ACC\uFE00","vartheta":"\u03D1","vartriangleleft":"\u22B2","vartriangleright":"\u22B3","vBar":"\u2AE8","Vbar":"\u2AEB","vBarv":"\u2AE9","Vcy":"\u0412","vcy":"\u0432","vdash":"\u22A2","vDash":"\u22A8","Vdash":"\u22A9","VDash":"\u22AB","Vdashl":"\u2AE6","veebar":"\u22BB","vee":"\u2228","Vee":"\u22C1","veeeq":"\u225A","vellip":"\u22EE","verbar":"|","Verbar":"\u2016","vert":"|","Vert":"\u2016","VerticalBar":"\u2223","VerticalLine":"|","VerticalSeparator":"\u2758","VerticalTilde":"\u2240","VeryThinSpace":"\u200A","Vfr":"\uD835\uDD19","vfr":"\uD835\uDD33","vltri":"\u22B2","vnsub":"\u2282\u20D2","vnsup":"\u2283\u20D2","Vopf":"\uD835\uDD4D","vopf":"\uD835\uDD67","vprop":"\u221D","vrtri":"\u22B3","Vscr":"\uD835\uDCB1","vscr":"\uD835\uDCCB","vsubnE":"\u2ACB\uFE00","vsubne":"\u228A\uFE00","vsupnE":"\u2ACC\uFE00","vsupne":"\u228B\uFE00","Vvdash":"\u22AA","vzigzag":"\u299A","Wcirc":"\u0174","wcirc":"\u0175","wedbar":"\u2A5F","wedge":"\u2227","Wedge":"\u22C0","wedgeq":"\u2259","weierp":"\u2118","Wfr":"\uD835\uDD1A","wfr":"\uD835\uDD34","Wopf":"\uD835\uDD4E","wopf":"\uD835\uDD68","wp":"\u2118","wr":"\u2240","wreath":"\u2240","Wscr":"\uD835\uDCB2","wscr":"\uD835\uDCCC","xcap":"\u22C2","xcirc":"\u25EF","xcup":"\u22C3","xdtri":"\u25BD","Xfr":"\uD835\uDD1B","xfr":"\uD835\uDD35","xharr":"\u27F7","xhArr":"\u27FA","Xi":"\u039E","xi":"\u03BE","xlarr":"\u27F5","xlArr":"\u27F8","xmap":"\u27FC","xnis":"\u22FB","xodot":"\u2A00","Xopf":"\uD835\uDD4F","xopf":"\uD835\uDD69","xoplus":"\u2A01","xotime":"\u2A02","xrarr":"\u27F6","xrArr":"\u27F9","Xscr":"\uD835\uDCB3","xscr":"\uD835\uDCCD","xsqcup":"\u2A06","xuplus":"\u2A04","xutri":"\u25B3","xvee":"\u22C1","xwedge":"\u22C0","Yacute":"\u00DD","yacute":"\u00FD","YAcy":"\u042F","yacy":"\u044F","Ycirc":"\u0176","ycirc":"\u0177","Ycy":"\u042B","ycy":"\u044B","yen":"\u00A5","Yfr":"\uD835\uDD1C","yfr":"\uD835\uDD36","YIcy":"\u0407","yicy":"\u0457","Yopf":"\uD835\uDD50","yopf":"\uD835\uDD6A","Yscr":"\uD835\uDCB4","yscr":"\uD835\uDCCE","YUcy":"\u042E","yucy":"\u044E","yuml":"\u00FF","Yuml":"\u0178","Zacute":"\u0179","zacute":"\u017A","Zcaron":"\u017D","zcaron":"\u017E","Zcy":"\u0417","zcy":"\u0437","Zdot":"\u017B","zdot":"\u017C","zeetrf":"\u2128","ZeroWidthSpace":"\u200B","Zeta":"\u0396","zeta":"\u03B6","zfr":"\uD835\uDD37","Zfr":"\u2128","ZHcy":"\u0416","zhcy":"\u0436","zigrarr":"\u21DD","zopf":"\uD835\uDD6B","Zopf":"\u2124","Zscr":"\uD835\uDCB5","zscr":"\uD835\uDCCF","zwj":"\u200D","zwnj":"\u200C"}
},{}],35:[function(require,module,exports){
module.exports={"Aacute":"\u00C1","aacute":"\u00E1","Acirc":"\u00C2","acirc":"\u00E2","acute":"\u00B4","AElig":"\u00C6","aelig":"\u00E6","Agrave":"\u00C0","agrave":"\u00E0","amp":"&","AMP":"&","Aring":"\u00C5","aring":"\u00E5","Atilde":"\u00C3","atilde":"\u00E3","Auml":"\u00C4","auml":"\u00E4","brvbar":"\u00A6","Ccedil":"\u00C7","ccedil":"\u00E7","cedil":"\u00B8","cent":"\u00A2","copy":"\u00A9","COPY":"\u00A9","curren":"\u00A4","deg":"\u00B0","divide":"\u00F7","Eacute":"\u00C9","eacute":"\u00E9","Ecirc":"\u00CA","ecirc":"\u00EA","Egrave":"\u00C8","egrave":"\u00E8","ETH":"\u00D0","eth":"\u00F0","Euml":"\u00CB","euml":"\u00EB","frac12":"\u00BD","frac14":"\u00BC","frac34":"\u00BE","gt":">","GT":">","Iacute":"\u00CD","iacute":"\u00ED","Icirc":"\u00CE","icirc":"\u00EE","iexcl":"\u00A1","Igrave":"\u00CC","igrave":"\u00EC","iquest":"\u00BF","Iuml":"\u00CF","iuml":"\u00EF","laquo":"\u00AB","lt":"<","LT":"<","macr":"\u00AF","micro":"\u00B5","middot":"\u00B7","nbsp":"\u00A0","not":"\u00AC","Ntilde":"\u00D1","ntilde":"\u00F1","Oacute":"\u00D3","oacute":"\u00F3","Ocirc":"\u00D4","ocirc":"\u00F4","Ograve":"\u00D2","ograve":"\u00F2","ordf":"\u00AA","ordm":"\u00BA","Oslash":"\u00D8","oslash":"\u00F8","Otilde":"\u00D5","otilde":"\u00F5","Ouml":"\u00D6","ouml":"\u00F6","para":"\u00B6","plusmn":"\u00B1","pound":"\u00A3","quot":"\"","QUOT":"\"","raquo":"\u00BB","reg":"\u00AE","REG":"\u00AE","sect":"\u00A7","shy":"\u00AD","sup1":"\u00B9","sup2":"\u00B2","sup3":"\u00B3","szlig":"\u00DF","THORN":"\u00DE","thorn":"\u00FE","times":"\u00D7","Uacute":"\u00DA","uacute":"\u00FA","Ucirc":"\u00DB","ucirc":"\u00FB","Ugrave":"\u00D9","ugrave":"\u00F9","uml":"\u00A8","Uuml":"\u00DC","uuml":"\u00FC","Yacute":"\u00DD","yacute":"\u00FD","yen":"\u00A5","yuml":"\u00FF"}
},{}],36:[function(require,module,exports){
module.exports={"amp":"&","apos":"'","gt":">","lt":"<","quot":"\""}

},{}],37:[function(require,module,exports){
var Parser = require("./Parser.js"),
    DomHandler = require("domhandler");

function defineProp(name, value){
	delete module.exports[name];
	module.exports[name] = value;
	return value;
}

module.exports = {
	Parser: Parser,
	Tokenizer: require("./Tokenizer.js"),
	ElementType: require("domelementtype"),
	DomHandler: DomHandler,
	get FeedHandler(){
		return defineProp("FeedHandler", require("./FeedHandler.js"));
	},
	get Stream(){
		return defineProp("Stream", require("./Stream.js"));
	},
	get WritableStream(){
		return defineProp("WritableStream", require("./WritableStream.js"));
	},
	get ProxyHandler(){
		return defineProp("ProxyHandler", require("./ProxyHandler.js"));
	},
	get DomUtils(){
		return defineProp("DomUtils", require("domutils"));
	},
	get CollectingHandler(){
		return defineProp("CollectingHandler", require("./CollectingHandler.js"));
	},
	// For legacy support
	DefaultHandler: DomHandler,
	get RssHandler(){
		return defineProp("RssHandler", this.FeedHandler);
	},
	//helper methods
	parseDOM: function(data, options) {
		var handler = new DomHandler(options);
		var parser = new Parser(handler, options);
		parser.end(data);
		return handler.dom;
	},
	parseFeed: function(feed, options){
		var handler = new module.exports.FeedHandler();
		var parser = new Parser(handler);
		parser.end(feed);
		return handler.dom;
	},
	createDomStream: function(cb, options, elementCb){
		var handler = new DomHandler(cb, options, elementCb);
		return new Parser(handler, options);
	},
	// List of all events that the parser emits
	EVENTS: { /* Format: eventname: number of arguments */
		attribute: 2,
		cdatastart: 0,
		cdataend: 0,
		text: 1,
		processinginstruction: 2,
		comment: 1,
		commentend: 0,
		closetag: 1,
		opentag: 2,
		opentagname: 1,
		error: 1,
		end: 0
	}
};

},{"./CollectingHandler.js":26,"./FeedHandler.js":27,"./Parser.js":28,"./ProxyHandler.js":29,"./Stream.js":30,"./Tokenizer.js":31,"./WritableStream.js":32,"domelementtype":38,"domhandler":39,"domutils":40}],38:[function(require,module,exports){
//Types of elements found in the DOM
module.exports = {
	Text: "text", //Text
	Directive: "directive", //<? ... ?>
	Comment: "comment", //<!-- ... -->
	Script: "script", //<script> tags
	Style: "style", //<style> tags
	Tag: "tag", //Any tag
	CDATA: "cdata", //<![CDATA[ ... ]]>

	isTag: function(elem){
		return elem.type === "tag" || elem.type === "script" || elem.type === "style";
	}
};
},{}],39:[function(require,module,exports){
var ElementType = require("domelementtype");

var re_whitespace = /\s+/g;

function DomHandler(callback, options, elementCB){
	if(typeof callback === "object"){
		elementCB = options;
		options = callback;
		callback = null;
	} else if(typeof options === "function"){
		elementCB = options;
		options = defaultOpts;
	}
	this._callback = callback;
	this._options = options || defaultOpts;
	this._elementCB = elementCB;
	this.dom = [];
	this._done = false;
	this._tagStack = [];
}

//default options
var defaultOpts = {
	normalizeWhitespace: false //Replace all whitespace with single spaces
};

//Resets the handler back to starting state
DomHandler.prototype.onreset = function(){
	DomHandler.call(this, this._callback, this._options, this._elementCB);
};

//Signals the handler that parsing is done
DomHandler.prototype.onend = function(){
	if(this._done) return;
	this._done = true;
	this._handleCallback(null);
};

DomHandler.prototype._handleCallback =
DomHandler.prototype.onerror = function(error){
	if(typeof this._callback === "function"){
		this._callback(error, this.dom);
	} else {
		if(error) throw error;
	}
};

DomHandler.prototype.onclosetag = function(name){
	//if(this._tagStack.pop().name !== name) this._handleCallback(Error("Tagname didn't match!"));
	var elem = this._tagStack.pop();
	if(this._elementCB) this._elementCB(elem);
};

DomHandler.prototype._addDomElement = function(element){
	var lastTag = this._tagStack[this._tagStack.length - 1];

	if(lastTag){
		lastTag.children.push(element);
	} else { //There aren't parent elements
		this.dom.push(element);
	}
};

DomHandler.prototype.onopentag = function(name, attribs){
	var lastTag = this._tagStack[this._tagStack.length - 1];

	var element = {
		type: name === "script" ? ElementType.Script : name === "style" ? ElementType.Style : ElementType.Tag,
		name: name,
		attribs: attribs,
		children: [],
		prev: null,
		next: null,
		parent: lastTag || null
	};

	if(lastTag){
		var idx = lastTag.children.length;
		while(idx > 0){
			if(ElementType.isTag(lastTag.children[--idx])){
				element.prev = lastTag.children[idx];
				lastTag.children[idx].next = element;
				break;
			}
		}
		lastTag.children.push(element);
	} else {
		this.dom.push(element);
	}

	this._tagStack.push(element);
};

DomHandler.prototype.ontext = function(data){
	//the ignoreWhitespace is officially dropped, but for now,
	//it's an alias for normalizeWhitespace
	var normalize = this._options.normalizeWhitespace || this._options.ignoreWhitespace;

	var lastTag;

	if(!this._tagStack.length && this.dom.length && (lastTag = this.dom[this.dom.length-1]).type === ElementType.Text){
		if(normalize){
			lastTag.data = (lastTag.data + data).replace(re_whitespace, " ");
		} else {
			lastTag.data += data;
		}
	} else {
		if(
			this._tagStack.length &&
			(lastTag = this._tagStack[this._tagStack.length - 1]) &&
			(lastTag = lastTag.children[lastTag.children.length - 1]) &&
			lastTag.type === ElementType.Text
		){
			if(normalize){
				lastTag.data = (lastTag.data + data).replace(re_whitespace, " ");
			} else {
				lastTag.data += data;
			}
		} else {
			if(normalize){
				data = data.replace(re_whitespace, " ");
			}

			this._addDomElement({
				data: data,
				type: ElementType.Text
			});
		}
	}
};

DomHandler.prototype.oncomment = function(data){
	var lastTag = this._tagStack[this._tagStack.length - 1];

	if(lastTag && lastTag.type === ElementType.Comment){
		lastTag.data += data;
		return;
	}

	var element = {
		data: data,
		type: ElementType.Comment
	};

	this._addDomElement(element);
	this._tagStack.push(element);
};

DomHandler.prototype.oncdatastart = function(){
	var element = {
		children: [{
			data: "",
			type: ElementType.Text
		}],
		type: ElementType.CDATA
	};

	this._addDomElement(element);
	this._tagStack.push(element);
};

DomHandler.prototype.oncommentend = DomHandler.prototype.oncdataend = function(){
	this._tagStack.pop();
};

DomHandler.prototype.onprocessinginstruction = function(name, data){
	this._addDomElement({
		name: name,
		data: data,
		type: ElementType.Directive
	});
};

module.exports = DomHandler;
},{"domelementtype":38}],40:[function(require,module,exports){
var ElementType = require("domelementtype"),
    DomUtils = module.exports;

var isTag = DomUtils.isTag = ElementType.isTag;

function getChildren(elem){
	return elem.children;
}
function getParent(elem){
	return elem.parent;
}
function getSiblings(elem){
	var parent = getParent(elem);
	return parent ? getChildren(parent) : [elem];
}
function getAttributeValue(elem, name){
	return elem.attribs && elem.attribs[name];
}
function hasAttrib(elem, name){
	return hasOwnProperty.call(elem.attribs, name);
}
function getName(elem){
	return elem.name;
}

DomUtils.getChildren = getChildren;
DomUtils.getParent = getParent;
DomUtils.getAttributeValue = getAttributeValue;
DomUtils.hasAttrib = hasAttrib;
DomUtils.getName = getName;
DomUtils.getSiblings = getSiblings;

function find(test, arr, recurse, limit){
	var result = [], childs;

	for(var i = 0, j = arr.length; i < j; i++){
		if(test(arr[i])){
			result.push(arr[i]);
			if(--limit <= 0) break;
		}

		childs = getChildren(arr[i]);
		if(recurse && childs && childs.length > 0){
			childs = find(test, childs, recurse, limit);
			result = result.concat(childs);
			limit -= childs.length;
			if(limit <= 0) break;
		}
	}

	return result;
}

function findOneChild(test, arr){
	for(var i = 0, l = arr.length; i < l; i++){
		if(test(arr[i])) return arr[i];
	}

	return null;
}

function findOne(test, arr){
	var elem = null;

	for(var i = 0, l = arr.length; i < l && !elem; i++){
		if(test(arr[i])){
			elem = arr[i];
		} else if(arr[i].children && arr[i].children.length > 0){
			elem = findOne(test, arr[i].children);
		}
	}

	return elem;
}

DomUtils.findOne = findOne;

function findAll(test, elems){
	var result = [];
	for(var i = 0, j = elems.length; i < j; i++){
		if(test(elems[i])) result.push(elems[i]);

		var childs = getChildren(elems[i]);
		if(childs && childs.length){
			result = result.concat(findAll(test, childs));
		}
	}
	return result;
}

DomUtils.findAll = findAll;

function filter(test, element, recurse, limit){
	if(!Array.isArray(element)) element = [element];

	if(typeof limit !== "number" || !isFinite(limit)){
		if(recurse === false){
			return element.filter(test);
		} else {
			return findAll(test, element);
		}
	} else if(limit === 1){
		if(recurse === false){
			element = findOneChild(test, element);
		} else {
			element = findOne(test, element);
		}
		return element ? [element] : [];
	} else {
		return find(test, element, recurse !== false, limit);
	}
}

DomUtils.filter = filter;

DomUtils.testElement = function(options, element){
	for(var key in options){
		if(!options.hasOwnProperty(key));
		else if(key === "tag_name"){
			if(!isTag(element) || !options.tag_name(element.name)){
				return false;
			}
		} else if(key === "tag_type"){
			if(!options.tag_type(element.type)) return false;
		} else if(key === "tag_contains"){
			if(isTag(element) || !options.tag_contains(element.data)){
				return false;
			}
		} else if(!element.attribs || !options[key](element.attribs[key])){
			return false;
		}
	}
	return true;
};

var Checks = {
	tag_name: function(name){
		if(typeof name === "function"){ 
			return function(elem){ return isTag(elem) && name(elem.name); };
		} else if(name === "*"){
			return isTag;
		} else {
			return function(elem){ return isTag(elem) && elem.name === name; };
		}
	},
	tag_type: function(type){
		if(typeof type === "function"){
			return function(elem){ return type(elem.type); };
		} else {
			return function(elem){ return elem.type === type; };
		}
	},
	tag_contains: function(data){
		if(typeof type === "function"){
			return function(elem){ return !isTag(elem) && data(elem.data); };
		} else {
			return function(elem){ return !isTag(elem) && elem.data === data; };
		}
	}
};

function getAttribCheck(attrib, value){
	if(typeof value === "function"){
		return function(elem){ return elem.attribs && value(elem.attribs[attrib]); };
	} else {
		return function(elem){ return elem.attribs && elem.attribs[attrib] === value; };
	}
}

DomUtils.getElements = function(options, element, recurse, limit){
	var funcs = [];
	for(var key in options){
		if(options.hasOwnProperty(key)){
			if(key in Checks) funcs.push(Checks[key](options[key]));
			else funcs.push(getAttribCheck(key, options[key]));
		}
	}

	if(funcs.length === 0) return [];
	if(funcs.length === 1) return filter(funcs[0], element, recurse, limit);
	return filter(
		function(elem){
			return funcs.some(function(func){ return func(elem); });
		},
		element, recurse, limit
	);
};

DomUtils.getElementById = function(id, element, recurse){
	if(!Array.isArray(element)) element = [element];
	return findOne(getAttribCheck("id", id), element, recurse !== false);
};

DomUtils.getElementsByTagName = function(name, element, recurse, limit){
	return filter(Checks.tag_name(name), element, recurse, limit);
};

DomUtils.getElementsByTagType = function(type, element, recurse, limit){
	return filter(Checks.tag_type(type), element, recurse, limit);
};

DomUtils.removeElement = function(elem){
	if(elem.prev) elem.prev.next = elem.next;
	if(elem.next) elem.next.prev = elem.prev;

	if(elem.parent){
		var childs = elem.parent.children;
		childs.splice(childs.lastIndexOf(elem), 1);
	}
};

DomUtils.replaceElement = function(elem, replacement){
	if(elem.prev){
		elem.prev.next = replacement;
		replacement.prev = elem.prev;
	}
	if(elem.next){
		elem.next.prev = replacement;
		replacement.next = elem.next;
	}
	if(elem.parent){
		var childs = elem.parent.children;
		childs.splice(childs.lastIndexOf(elem), 1, replacement);
		replacement.parent = elem.parent;
	}
};

DomUtils.getInnerHTML = function(elem){
	if(!elem.children) return "";

	var childs = elem.children,
		childNum = childs.length,
		ret = "";

	for(var i = 0; i < childNum; i++){
		ret += DomUtils.getOuterHTML(childs[i]);
	}

	return ret;
};

//boolean attributes without a value (taken from MatthewMueller/cheerio)
var booleanAttribs = {
	__proto__: null,
	async: true,
	autofocus: true,
	autoplay: true,
	checked: true,
	controls: true,
	defer: true,
	disabled: true,
	hidden: true,
	loop: true,
	multiple: true,
	open: true,
	readonly: true,
	required: true,
	scoped: true,
	selected: true,
	"/": true //TODO when is this required?
};

var emptyTags = {
	__proto__: null,
	area: true,
	base: true,
	basefont: true,
	br: true,
	col: true,
	frame: true,
	hr: true,
	img: true,
	input: true,
	isindex: true,
	link: true,
	meta: true,
	param: true,
	embed: true
};

DomUtils.getOuterHTML = function(elem){
	var type = elem.type;

	if(type === ElementType.Text) return elem.data;
	if(type === ElementType.Comment) return "<!--" + elem.data + "-->";
	if(type === ElementType.Directive) return "<" + elem.data + ">";
	if(type === ElementType.CDATA) return "<!CDATA " + DomUtils.getInnerHTML(elem) + "]]>";

	var ret = "<" + elem.name;
	if("attribs" in elem){
		for(var attr in elem.attribs){
			if(elem.attribs.hasOwnProperty(attr)){
				ret += " " + attr;
				var value = elem.attribs[attr];
				if(!value){
					if( !(attr in booleanAttribs) ){
						ret += '=""';
					}
				} else {
					ret += '="' + value + '"';
				}
			}
		}
	}

	if (elem.name in emptyTags && elem.children.length === 0) {
		return ret + " />";
	} else {
		return ret + ">" + DomUtils.getInnerHTML(elem) + "</" + elem.name + ">";
	}
};

DomUtils.getText = function getText(elem){
	if(Array.isArray(elem)) return elem.map(getText).join("");
	if(isTag(elem) || elem.type === ElementType.CDATA) return getText(elem.children);
	if(elem.type === ElementType.Text) return elem.data;
	return "";
};

},{"domelementtype":38}],41:[function(require,module,exports){
(function (process){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

// a duplex stream is just a stream that is both readable and writable.
// Since JS doesn't have multiple prototypal inheritance, this class
// prototypally inherits from Readable, and then parasitically from
// Writable.

module.exports = Duplex;

/*<replacement>*/
var objectKeys = Object.keys || function (obj) {
  var keys = [];
  for (var key in obj) keys.push(key);
  return keys;
}
/*</replacement>*/


/*<replacement>*/
var util = require('core-util-is');
util.inherits = require('inherits');
/*</replacement>*/

var Readable = require('./_stream_readable');
var Writable = require('./_stream_writable');

util.inherits(Duplex, Readable);

forEach(objectKeys(Writable.prototype), function(method) {
  if (!Duplex.prototype[method])
    Duplex.prototype[method] = Writable.prototype[method];
});

function Duplex(options) {
  if (!(this instanceof Duplex))
    return new Duplex(options);

  Readable.call(this, options);
  Writable.call(this, options);

  if (options && options.readable === false)
    this.readable = false;

  if (options && options.writable === false)
    this.writable = false;

  this.allowHalfOpen = true;
  if (options && options.allowHalfOpen === false)
    this.allowHalfOpen = false;

  this.once('end', onend);
}

// the no-half-open enforcer
function onend() {
  // if we allow half-open state, or if the writable side ended,
  // then we're ok.
  if (this.allowHalfOpen || this._writableState.ended)
    return;

  // no more data can be written.
  // But allow more writes to happen in this tick.
  process.nextTick(this.end.bind(this));
}

function forEach (xs, f) {
  for (var i = 0, l = xs.length; i < l; i++) {
    f(xs[i], i);
  }
}

}).call(this,require("/home/noah/Projects/shimi_ima/node_modules/browserify/node_modules/insert-module-globals/node_modules/process/browser.js"))
},{"./_stream_readable":43,"./_stream_writable":45,"/home/noah/Projects/shimi_ima/node_modules/browserify/node_modules/insert-module-globals/node_modules/process/browser.js":12,"core-util-is":46,"inherits":47}],42:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

// a passthrough stream.
// basically just the most minimal sort of Transform stream.
// Every written chunk gets output as-is.

module.exports = PassThrough;

var Transform = require('./_stream_transform');

/*<replacement>*/
var util = require('core-util-is');
util.inherits = require('inherits');
/*</replacement>*/

util.inherits(PassThrough, Transform);

function PassThrough(options) {
  if (!(this instanceof PassThrough))
    return new PassThrough(options);

  Transform.call(this, options);
}

PassThrough.prototype._transform = function(chunk, encoding, cb) {
  cb(null, chunk);
};

},{"./_stream_transform":44,"core-util-is":46,"inherits":47}],43:[function(require,module,exports){
(function (process){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

module.exports = Readable;

/*<replacement>*/
var isArray = require('isarray');
/*</replacement>*/


/*<replacement>*/
var Buffer = require('buffer').Buffer;
/*</replacement>*/

Readable.ReadableState = ReadableState;

var EE = require('events').EventEmitter;

/*<replacement>*/
if (!EE.listenerCount) EE.listenerCount = function(emitter, type) {
  return emitter.listeners(type).length;
};
/*</replacement>*/

var Stream = require('stream');

/*<replacement>*/
var util = require('core-util-is');
util.inherits = require('inherits');
/*</replacement>*/

var StringDecoder;

util.inherits(Readable, Stream);

function ReadableState(options, stream) {
  options = options || {};

  // the point at which it stops calling _read() to fill the buffer
  // Note: 0 is a valid value, means "don't call _read preemptively ever"
  var hwm = options.highWaterMark;
  this.highWaterMark = (hwm || hwm === 0) ? hwm : 16 * 1024;

  // cast to ints.
  this.highWaterMark = ~~this.highWaterMark;

  this.buffer = [];
  this.length = 0;
  this.pipes = null;
  this.pipesCount = 0;
  this.flowing = false;
  this.ended = false;
  this.endEmitted = false;
  this.reading = false;

  // In streams that never have any data, and do push(null) right away,
  // the consumer can miss the 'end' event if they do some I/O before
  // consuming the stream.  So, we don't emit('end') until some reading
  // happens.
  this.calledRead = false;

  // a flag to be able to tell if the onwrite cb is called immediately,
  // or on a later tick.  We set this to true at first, becuase any
  // actions that shouldn't happen until "later" should generally also
  // not happen before the first write call.
  this.sync = true;

  // whenever we return null, then we set a flag to say
  // that we're awaiting a 'readable' event emission.
  this.needReadable = false;
  this.emittedReadable = false;
  this.readableListening = false;


  // object stream flag. Used to make read(n) ignore n and to
  // make all the buffer merging and length checks go away
  this.objectMode = !!options.objectMode;

  // Crypto is kind of old and crusty.  Historically, its default string
  // encoding is 'binary' so we have to make this configurable.
  // Everything else in the universe uses 'utf8', though.
  this.defaultEncoding = options.defaultEncoding || 'utf8';

  // when piping, we only care about 'readable' events that happen
  // after read()ing all the bytes and not getting any pushback.
  this.ranOut = false;

  // the number of writers that are awaiting a drain event in .pipe()s
  this.awaitDrain = 0;

  // if true, a maybeReadMore has been scheduled
  this.readingMore = false;

  this.decoder = null;
  this.encoding = null;
  if (options.encoding) {
    if (!StringDecoder)
      StringDecoder = require('string_decoder/').StringDecoder;
    this.decoder = new StringDecoder(options.encoding);
    this.encoding = options.encoding;
  }
}

function Readable(options) {
  if (!(this instanceof Readable))
    return new Readable(options);

  this._readableState = new ReadableState(options, this);

  // legacy
  this.readable = true;

  Stream.call(this);
}

// Manually shove something into the read() buffer.
// This returns true if the highWaterMark has not been hit yet,
// similar to how Writable.write() returns true if you should
// write() some more.
Readable.prototype.push = function(chunk, encoding) {
  var state = this._readableState;

  if (typeof chunk === 'string' && !state.objectMode) {
    encoding = encoding || state.defaultEncoding;
    if (encoding !== state.encoding) {
      chunk = new Buffer(chunk, encoding);
      encoding = '';
    }
  }

  return readableAddChunk(this, state, chunk, encoding, false);
};

// Unshift should *always* be something directly out of read()
Readable.prototype.unshift = function(chunk) {
  var state = this._readableState;
  return readableAddChunk(this, state, chunk, '', true);
};

function readableAddChunk(stream, state, chunk, encoding, addToFront) {
  var er = chunkInvalid(state, chunk);
  if (er) {
    stream.emit('error', er);
  } else if (chunk === null || chunk === undefined) {
    state.reading = false;
    if (!state.ended)
      onEofChunk(stream, state);
  } else if (state.objectMode || chunk && chunk.length > 0) {
    if (state.ended && !addToFront) {
      var e = new Error('stream.push() after EOF');
      stream.emit('error', e);
    } else if (state.endEmitted && addToFront) {
      var e = new Error('stream.unshift() after end event');
      stream.emit('error', e);
    } else {
      if (state.decoder && !addToFront && !encoding)
        chunk = state.decoder.write(chunk);

      // update the buffer info.
      state.length += state.objectMode ? 1 : chunk.length;
      if (addToFront) {
        state.buffer.unshift(chunk);
      } else {
        state.reading = false;
        state.buffer.push(chunk);
      }

      if (state.needReadable)
        emitReadable(stream);

      maybeReadMore(stream, state);
    }
  } else if (!addToFront) {
    state.reading = false;
  }

  return needMoreData(state);
}



// if it's past the high water mark, we can push in some more.
// Also, if we have no data yet, we can stand some
// more bytes.  This is to work around cases where hwm=0,
// such as the repl.  Also, if the push() triggered a
// readable event, and the user called read(largeNumber) such that
// needReadable was set, then we ought to push more, so that another
// 'readable' event will be triggered.
function needMoreData(state) {
  return !state.ended &&
         (state.needReadable ||
          state.length < state.highWaterMark ||
          state.length === 0);
}

// backwards compatibility.
Readable.prototype.setEncoding = function(enc) {
  if (!StringDecoder)
    StringDecoder = require('string_decoder/').StringDecoder;
  this._readableState.decoder = new StringDecoder(enc);
  this._readableState.encoding = enc;
};

// Don't raise the hwm > 128MB
var MAX_HWM = 0x800000;
function roundUpToNextPowerOf2(n) {
  if (n >= MAX_HWM) {
    n = MAX_HWM;
  } else {
    // Get the next highest power of 2
    n--;
    for (var p = 1; p < 32; p <<= 1) n |= n >> p;
    n++;
  }
  return n;
}

function howMuchToRead(n, state) {
  if (state.length === 0 && state.ended)
    return 0;

  if (state.objectMode)
    return n === 0 ? 0 : 1;

  if (isNaN(n) || n === null) {
    // only flow one buffer at a time
    if (state.flowing && state.buffer.length)
      return state.buffer[0].length;
    else
      return state.length;
  }

  if (n <= 0)
    return 0;

  // If we're asking for more than the target buffer level,
  // then raise the water mark.  Bump up to the next highest
  // power of 2, to prevent increasing it excessively in tiny
  // amounts.
  if (n > state.highWaterMark)
    state.highWaterMark = roundUpToNextPowerOf2(n);

  // don't have that much.  return null, unless we've ended.
  if (n > state.length) {
    if (!state.ended) {
      state.needReadable = true;
      return 0;
    } else
      return state.length;
  }

  return n;
}

// you can override either this method, or the async _read(n) below.
Readable.prototype.read = function(n) {
  var state = this._readableState;
  state.calledRead = true;
  var nOrig = n;

  if (typeof n !== 'number' || n > 0)
    state.emittedReadable = false;

  // if we're doing read(0) to trigger a readable event, but we
  // already have a bunch of data in the buffer, then just trigger
  // the 'readable' event and move on.
  if (n === 0 &&
      state.needReadable &&
      (state.length >= state.highWaterMark || state.ended)) {
    emitReadable(this);
    return null;
  }

  n = howMuchToRead(n, state);

  // if we've ended, and we're now clear, then finish it up.
  if (n === 0 && state.ended) {
    if (state.length === 0)
      endReadable(this);
    return null;
  }

  // All the actual chunk generation logic needs to be
  // *below* the call to _read.  The reason is that in certain
  // synthetic stream cases, such as passthrough streams, _read
  // may be a completely synchronous operation which may change
  // the state of the read buffer, providing enough data when
  // before there was *not* enough.
  //
  // So, the steps are:
  // 1. Figure out what the state of things will be after we do
  // a read from the buffer.
  //
  // 2. If that resulting state will trigger a _read, then call _read.
  // Note that this may be asynchronous, or synchronous.  Yes, it is
  // deeply ugly to write APIs this way, but that still doesn't mean
  // that the Readable class should behave improperly, as streams are
  // designed to be sync/async agnostic.
  // Take note if the _read call is sync or async (ie, if the read call
  // has returned yet), so that we know whether or not it's safe to emit
  // 'readable' etc.
  //
  // 3. Actually pull the requested chunks out of the buffer and return.

  // if we need a readable event, then we need to do some reading.
  var doRead = state.needReadable;

  // if we currently have less than the highWaterMark, then also read some
  if (state.length - n <= state.highWaterMark)
    doRead = true;

  // however, if we've ended, then there's no point, and if we're already
  // reading, then it's unnecessary.
  if (state.ended || state.reading)
    doRead = false;

  if (doRead) {
    state.reading = true;
    state.sync = true;
    // if the length is currently zero, then we *need* a readable event.
    if (state.length === 0)
      state.needReadable = true;
    // call internal read method
    this._read(state.highWaterMark);
    state.sync = false;
  }

  // If _read called its callback synchronously, then `reading`
  // will be false, and we need to re-evaluate how much data we
  // can return to the user.
  if (doRead && !state.reading)
    n = howMuchToRead(nOrig, state);

  var ret;
  if (n > 0)
    ret = fromList(n, state);
  else
    ret = null;

  if (ret === null) {
    state.needReadable = true;
    n = 0;
  }

  state.length -= n;

  // If we have nothing in the buffer, then we want to know
  // as soon as we *do* get something into the buffer.
  if (state.length === 0 && !state.ended)
    state.needReadable = true;

  // If we happened to read() exactly the remaining amount in the
  // buffer, and the EOF has been seen at this point, then make sure
  // that we emit 'end' on the very next tick.
  if (state.ended && !state.endEmitted && state.length === 0)
    endReadable(this);

  return ret;
};

function chunkInvalid(state, chunk) {
  var er = null;
  if (!Buffer.isBuffer(chunk) &&
      'string' !== typeof chunk &&
      chunk !== null &&
      chunk !== undefined &&
      !state.objectMode &&
      !er) {
    er = new TypeError('Invalid non-string/buffer chunk');
  }
  return er;
}


function onEofChunk(stream, state) {
  if (state.decoder && !state.ended) {
    var chunk = state.decoder.end();
    if (chunk && chunk.length) {
      state.buffer.push(chunk);
      state.length += state.objectMode ? 1 : chunk.length;
    }
  }
  state.ended = true;

  // if we've ended and we have some data left, then emit
  // 'readable' now to make sure it gets picked up.
  if (state.length > 0)
    emitReadable(stream);
  else
    endReadable(stream);
}

// Don't emit readable right away in sync mode, because this can trigger
// another read() call => stack overflow.  This way, it might trigger
// a nextTick recursion warning, but that's not so bad.
function emitReadable(stream) {
  var state = stream._readableState;
  state.needReadable = false;
  if (state.emittedReadable)
    return;

  state.emittedReadable = true;
  if (state.sync)
    process.nextTick(function() {
      emitReadable_(stream);
    });
  else
    emitReadable_(stream);
}

function emitReadable_(stream) {
  stream.emit('readable');
}


// at this point, the user has presumably seen the 'readable' event,
// and called read() to consume some data.  that may have triggered
// in turn another _read(n) call, in which case reading = true if
// it's in progress.
// However, if we're not ended, or reading, and the length < hwm,
// then go ahead and try to read some more preemptively.
function maybeReadMore(stream, state) {
  if (!state.readingMore) {
    state.readingMore = true;
    process.nextTick(function() {
      maybeReadMore_(stream, state);
    });
  }
}

function maybeReadMore_(stream, state) {
  var len = state.length;
  while (!state.reading && !state.flowing && !state.ended &&
         state.length < state.highWaterMark) {
    stream.read(0);
    if (len === state.length)
      // didn't get any data, stop spinning.
      break;
    else
      len = state.length;
  }
  state.readingMore = false;
}

// abstract method.  to be overridden in specific implementation classes.
// call cb(er, data) where data is <= n in length.
// for virtual (non-string, non-buffer) streams, "length" is somewhat
// arbitrary, and perhaps not very meaningful.
Readable.prototype._read = function(n) {
  this.emit('error', new Error('not implemented'));
};

Readable.prototype.pipe = function(dest, pipeOpts) {
  var src = this;
  var state = this._readableState;

  switch (state.pipesCount) {
    case 0:
      state.pipes = dest;
      break;
    case 1:
      state.pipes = [state.pipes, dest];
      break;
    default:
      state.pipes.push(dest);
      break;
  }
  state.pipesCount += 1;

  var doEnd = (!pipeOpts || pipeOpts.end !== false) &&
              dest !== process.stdout &&
              dest !== process.stderr;

  var endFn = doEnd ? onend : cleanup;
  if (state.endEmitted)
    process.nextTick(endFn);
  else
    src.once('end', endFn);

  dest.on('unpipe', onunpipe);
  function onunpipe(readable) {
    if (readable !== src) return;
    cleanup();
  }

  function onend() {
    dest.end();
  }

  // when the dest drains, it reduces the awaitDrain counter
  // on the source.  This would be more elegant with a .once()
  // handler in flow(), but adding and removing repeatedly is
  // too slow.
  var ondrain = pipeOnDrain(src);
  dest.on('drain', ondrain);

  function cleanup() {
    // cleanup event handlers once the pipe is broken
    dest.removeListener('close', onclose);
    dest.removeListener('finish', onfinish);
    dest.removeListener('drain', ondrain);
    dest.removeListener('error', onerror);
    dest.removeListener('unpipe', onunpipe);
    src.removeListener('end', onend);
    src.removeListener('end', cleanup);

    // if the reader is waiting for a drain event from this
    // specific writer, then it would cause it to never start
    // flowing again.
    // So, if this is awaiting a drain, then we just call it now.
    // If we don't know, then assume that we are waiting for one.
    if (!dest._writableState || dest._writableState.needDrain)
      ondrain();
  }

  // if the dest has an error, then stop piping into it.
  // however, don't suppress the throwing behavior for this.
  function onerror(er) {
    unpipe();
    dest.removeListener('error', onerror);
    if (EE.listenerCount(dest, 'error') === 0)
      dest.emit('error', er);
  }
  // This is a brutally ugly hack to make sure that our error handler
  // is attached before any userland ones.  NEVER DO THIS.
  if (!dest._events || !dest._events.error)
    dest.on('error', onerror);
  else if (isArray(dest._events.error))
    dest._events.error.unshift(onerror);
  else
    dest._events.error = [onerror, dest._events.error];



  // Both close and finish should trigger unpipe, but only once.
  function onclose() {
    dest.removeListener('finish', onfinish);
    unpipe();
  }
  dest.once('close', onclose);
  function onfinish() {
    dest.removeListener('close', onclose);
    unpipe();
  }
  dest.once('finish', onfinish);

  function unpipe() {
    src.unpipe(dest);
  }

  // tell the dest that it's being piped to
  dest.emit('pipe', src);

  // start the flow if it hasn't been started already.
  if (!state.flowing) {
    // the handler that waits for readable events after all
    // the data gets sucked out in flow.
    // This would be easier to follow with a .once() handler
    // in flow(), but that is too slow.
    this.on('readable', pipeOnReadable);

    state.flowing = true;
    process.nextTick(function() {
      flow(src);
    });
  }

  return dest;
};

function pipeOnDrain(src) {
  return function() {
    var dest = this;
    var state = src._readableState;
    state.awaitDrain--;
    if (state.awaitDrain === 0)
      flow(src);
  };
}

function flow(src) {
  var state = src._readableState;
  var chunk;
  state.awaitDrain = 0;

  function write(dest, i, list) {
    var written = dest.write(chunk);
    if (false === written) {
      state.awaitDrain++;
    }
  }

  while (state.pipesCount && null !== (chunk = src.read())) {

    if (state.pipesCount === 1)
      write(state.pipes, 0, null);
    else
      forEach(state.pipes, write);

    src.emit('data', chunk);

    // if anyone needs a drain, then we have to wait for that.
    if (state.awaitDrain > 0)
      return;
  }

  // if every destination was unpiped, either before entering this
  // function, or in the while loop, then stop flowing.
  //
  // NB: This is a pretty rare edge case.
  if (state.pipesCount === 0) {
    state.flowing = false;

    // if there were data event listeners added, then switch to old mode.
    if (EE.listenerCount(src, 'data') > 0)
      emitDataEvents(src);
    return;
  }

  // at this point, no one needed a drain, so we just ran out of data
  // on the next readable event, start it over again.
  state.ranOut = true;
}

function pipeOnReadable() {
  if (this._readableState.ranOut) {
    this._readableState.ranOut = false;
    flow(this);
  }
}


Readable.prototype.unpipe = function(dest) {
  var state = this._readableState;

  // if we're not piping anywhere, then do nothing.
  if (state.pipesCount === 0)
    return this;

  // just one destination.  most common case.
  if (state.pipesCount === 1) {
    // passed in one, but it's not the right one.
    if (dest && dest !== state.pipes)
      return this;

    if (!dest)
      dest = state.pipes;

    // got a match.
    state.pipes = null;
    state.pipesCount = 0;
    this.removeListener('readable', pipeOnReadable);
    state.flowing = false;
    if (dest)
      dest.emit('unpipe', this);
    return this;
  }

  // slow case. multiple pipe destinations.

  if (!dest) {
    // remove all.
    var dests = state.pipes;
    var len = state.pipesCount;
    state.pipes = null;
    state.pipesCount = 0;
    this.removeListener('readable', pipeOnReadable);
    state.flowing = false;

    for (var i = 0; i < len; i++)
      dests[i].emit('unpipe', this);
    return this;
  }

  // try to find the right one.
  var i = indexOf(state.pipes, dest);
  if (i === -1)
    return this;

  state.pipes.splice(i, 1);
  state.pipesCount -= 1;
  if (state.pipesCount === 1)
    state.pipes = state.pipes[0];

  dest.emit('unpipe', this);

  return this;
};

// set up data events if they are asked for
// Ensure readable listeners eventually get something
Readable.prototype.on = function(ev, fn) {
  var res = Stream.prototype.on.call(this, ev, fn);

  if (ev === 'data' && !this._readableState.flowing)
    emitDataEvents(this);

  if (ev === 'readable' && this.readable) {
    var state = this._readableState;
    if (!state.readableListening) {
      state.readableListening = true;
      state.emittedReadable = false;
      state.needReadable = true;
      if (!state.reading) {
        this.read(0);
      } else if (state.length) {
        emitReadable(this, state);
      }
    }
  }

  return res;
};
Readable.prototype.addListener = Readable.prototype.on;

// pause() and resume() are remnants of the legacy readable stream API
// If the user uses them, then switch into old mode.
Readable.prototype.resume = function() {
  emitDataEvents(this);
  this.read(0);
  this.emit('resume');
};

Readable.prototype.pause = function() {
  emitDataEvents(this, true);
  this.emit('pause');
};

function emitDataEvents(stream, startPaused) {
  var state = stream._readableState;

  if (state.flowing) {
    // https://github.com/isaacs/readable-stream/issues/16
    throw new Error('Cannot switch to old mode now.');
  }

  var paused = startPaused || false;
  var readable = false;

  // convert to an old-style stream.
  stream.readable = true;
  stream.pipe = Stream.prototype.pipe;
  stream.on = stream.addListener = Stream.prototype.on;

  stream.on('readable', function() {
    readable = true;

    var c;
    while (!paused && (null !== (c = stream.read())))
      stream.emit('data', c);

    if (c === null) {
      readable = false;
      stream._readableState.needReadable = true;
    }
  });

  stream.pause = function() {
    paused = true;
    this.emit('pause');
  };

  stream.resume = function() {
    paused = false;
    if (readable)
      process.nextTick(function() {
        stream.emit('readable');
      });
    else
      this.read(0);
    this.emit('resume');
  };

  // now make it start, just in case it hadn't already.
  stream.emit('readable');
}

// wrap an old-style stream as the async data source.
// This is *not* part of the readable stream interface.
// It is an ugly unfortunate mess of history.
Readable.prototype.wrap = function(stream) {
  var state = this._readableState;
  var paused = false;

  var self = this;
  stream.on('end', function() {
    if (state.decoder && !state.ended) {
      var chunk = state.decoder.end();
      if (chunk && chunk.length)
        self.push(chunk);
    }

    self.push(null);
  });

  stream.on('data', function(chunk) {
    if (state.decoder)
      chunk = state.decoder.write(chunk);
    if (!chunk || !state.objectMode && !chunk.length)
      return;

    var ret = self.push(chunk);
    if (!ret) {
      paused = true;
      stream.pause();
    }
  });

  // proxy all the other methods.
  // important when wrapping filters and duplexes.
  for (var i in stream) {
    if (typeof stream[i] === 'function' &&
        typeof this[i] === 'undefined') {
      this[i] = function(method) { return function() {
        return stream[method].apply(stream, arguments);
      }}(i);
    }
  }

  // proxy certain important events.
  var events = ['error', 'close', 'destroy', 'pause', 'resume'];
  forEach(events, function(ev) {
    stream.on(ev, self.emit.bind(self, ev));
  });

  // when we try to consume some more bytes, simply unpause the
  // underlying stream.
  self._read = function(n) {
    if (paused) {
      paused = false;
      stream.resume();
    }
  };

  return self;
};



// exposed for testing purposes only.
Readable._fromList = fromList;

// Pluck off n bytes from an array of buffers.
// Length is the combined lengths of all the buffers in the list.
function fromList(n, state) {
  var list = state.buffer;
  var length = state.length;
  var stringMode = !!state.decoder;
  var objectMode = !!state.objectMode;
  var ret;

  // nothing in the list, definitely empty.
  if (list.length === 0)
    return null;

  if (length === 0)
    ret = null;
  else if (objectMode)
    ret = list.shift();
  else if (!n || n >= length) {
    // read it all, truncate the array.
    if (stringMode)
      ret = list.join('');
    else
      ret = Buffer.concat(list, length);
    list.length = 0;
  } else {
    // read just some of it.
    if (n < list[0].length) {
      // just take a part of the first list item.
      // slice is the same for buffers and strings.
      var buf = list[0];
      ret = buf.slice(0, n);
      list[0] = buf.slice(n);
    } else if (n === list[0].length) {
      // first list is a perfect match
      ret = list.shift();
    } else {
      // complex case.
      // we have enough to cover it, but it spans past the first buffer.
      if (stringMode)
        ret = '';
      else
        ret = new Buffer(n);

      var c = 0;
      for (var i = 0, l = list.length; i < l && c < n; i++) {
        var buf = list[0];
        var cpy = Math.min(n - c, buf.length);

        if (stringMode)
          ret += buf.slice(0, cpy);
        else
          buf.copy(ret, c, 0, cpy);

        if (cpy < buf.length)
          list[0] = buf.slice(cpy);
        else
          list.shift();

        c += cpy;
      }
    }
  }

  return ret;
}

function endReadable(stream) {
  var state = stream._readableState;

  // If we get here before consuming all the bytes, then that is a
  // bug in node.  Should never happen.
  if (state.length > 0)
    throw new Error('endReadable called on non-empty stream');

  if (!state.endEmitted && state.calledRead) {
    state.ended = true;
    process.nextTick(function() {
      // Check that we didn't get one last unshift.
      if (!state.endEmitted && state.length === 0) {
        state.endEmitted = true;
        stream.readable = false;
        stream.emit('end');
      }
    });
  }
}

function forEach (xs, f) {
  for (var i = 0, l = xs.length; i < l; i++) {
    f(xs[i], i);
  }
}

function indexOf (xs, x) {
  for (var i = 0, l = xs.length; i < l; i++) {
    if (xs[i] === x) return i;
  }
  return -1;
}

}).call(this,require("/home/noah/Projects/shimi_ima/node_modules/browserify/node_modules/insert-module-globals/node_modules/process/browser.js"))
},{"/home/noah/Projects/shimi_ima/node_modules/browserify/node_modules/insert-module-globals/node_modules/process/browser.js":12,"buffer":1,"core-util-is":46,"events":10,"inherits":47,"isarray":48,"stream":14,"string_decoder/":49}],44:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.


// a transform stream is a readable/writable stream where you do
// something with the data.  Sometimes it's called a "filter",
// but that's not a great name for it, since that implies a thing where
// some bits pass through, and others are simply ignored.  (That would
// be a valid example of a transform, of course.)
//
// While the output is causally related to the input, it's not a
// necessarily symmetric or synchronous transformation.  For example,
// a zlib stream might take multiple plain-text writes(), and then
// emit a single compressed chunk some time in the future.
//
// Here's how this works:
//
// The Transform stream has all the aspects of the readable and writable
// stream classes.  When you write(chunk), that calls _write(chunk,cb)
// internally, and returns false if there's a lot of pending writes
// buffered up.  When you call read(), that calls _read(n) until
// there's enough pending readable data buffered up.
//
// In a transform stream, the written data is placed in a buffer.  When
// _read(n) is called, it transforms the queued up data, calling the
// buffered _write cb's as it consumes chunks.  If consuming a single
// written chunk would result in multiple output chunks, then the first
// outputted bit calls the readcb, and subsequent chunks just go into
// the read buffer, and will cause it to emit 'readable' if necessary.
//
// This way, back-pressure is actually determined by the reading side,
// since _read has to be called to start processing a new chunk.  However,
// a pathological inflate type of transform can cause excessive buffering
// here.  For example, imagine a stream where every byte of input is
// interpreted as an integer from 0-255, and then results in that many
// bytes of output.  Writing the 4 bytes {ff,ff,ff,ff} would result in
// 1kb of data being output.  In this case, you could write a very small
// amount of input, and end up with a very large amount of output.  In
// such a pathological inflating mechanism, there'd be no way to tell
// the system to stop doing the transform.  A single 4MB write could
// cause the system to run out of memory.
//
// However, even in such a pathological case, only a single written chunk
// would be consumed, and then the rest would wait (un-transformed) until
// the results of the previous transformed chunk were consumed.

module.exports = Transform;

var Duplex = require('./_stream_duplex');

/*<replacement>*/
var util = require('core-util-is');
util.inherits = require('inherits');
/*</replacement>*/

util.inherits(Transform, Duplex);


function TransformState(options, stream) {
  this.afterTransform = function(er, data) {
    return afterTransform(stream, er, data);
  };

  this.needTransform = false;
  this.transforming = false;
  this.writecb = null;
  this.writechunk = null;
}

function afterTransform(stream, er, data) {
  var ts = stream._transformState;
  ts.transforming = false;

  var cb = ts.writecb;

  if (!cb)
    return stream.emit('error', new Error('no writecb in Transform class'));

  ts.writechunk = null;
  ts.writecb = null;

  if (data !== null && data !== undefined)
    stream.push(data);

  if (cb)
    cb(er);

  var rs = stream._readableState;
  rs.reading = false;
  if (rs.needReadable || rs.length < rs.highWaterMark) {
    stream._read(rs.highWaterMark);
  }
}


function Transform(options) {
  if (!(this instanceof Transform))
    return new Transform(options);

  Duplex.call(this, options);

  var ts = this._transformState = new TransformState(options, this);

  // when the writable side finishes, then flush out anything remaining.
  var stream = this;

  // start out asking for a readable event once data is transformed.
  this._readableState.needReadable = true;

  // we have implemented the _read method, and done the other things
  // that Readable wants before the first _read call, so unset the
  // sync guard flag.
  this._readableState.sync = false;

  this.once('finish', function() {
    if ('function' === typeof this._flush)
      this._flush(function(er) {
        done(stream, er);
      });
    else
      done(stream);
  });
}

Transform.prototype.push = function(chunk, encoding) {
  this._transformState.needTransform = false;
  return Duplex.prototype.push.call(this, chunk, encoding);
};

// This is the part where you do stuff!
// override this function in implementation classes.
// 'chunk' is an input chunk.
//
// Call `push(newChunk)` to pass along transformed output
// to the readable side.  You may call 'push' zero or more times.
//
// Call `cb(err)` when you are done with this chunk.  If you pass
// an error, then that'll put the hurt on the whole operation.  If you
// never call cb(), then you'll never get another chunk.
Transform.prototype._transform = function(chunk, encoding, cb) {
  throw new Error('not implemented');
};

Transform.prototype._write = function(chunk, encoding, cb) {
  var ts = this._transformState;
  ts.writecb = cb;
  ts.writechunk = chunk;
  ts.writeencoding = encoding;
  if (!ts.transforming) {
    var rs = this._readableState;
    if (ts.needTransform ||
        rs.needReadable ||
        rs.length < rs.highWaterMark)
      this._read(rs.highWaterMark);
  }
};

// Doesn't matter what the args are here.
// _transform does all the work.
// That we got here means that the readable side wants more data.
Transform.prototype._read = function(n) {
  var ts = this._transformState;

  if (ts.writechunk !== null && ts.writecb && !ts.transforming) {
    ts.transforming = true;
    this._transform(ts.writechunk, ts.writeencoding, ts.afterTransform);
  } else {
    // mark that we need a transform, so that any data that comes in
    // will get processed, now that we've asked for it.
    ts.needTransform = true;
  }
};


function done(stream, er) {
  if (er)
    return stream.emit('error', er);

  // if there's nothing in the write buffer, then that means
  // that nothing more will ever be provided
  var ws = stream._writableState;
  var rs = stream._readableState;
  var ts = stream._transformState;

  if (ws.length)
    throw new Error('calling transform done when ws.length != 0');

  if (ts.transforming)
    throw new Error('calling transform done when still transforming');

  return stream.push(null);
}

},{"./_stream_duplex":41,"core-util-is":46,"inherits":47}],45:[function(require,module,exports){
(function (process){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

// A bit simpler than readable streams.
// Implement an async ._write(chunk, cb), and it'll handle all
// the drain event emission and buffering.

module.exports = Writable;

/*<replacement>*/
var Buffer = require('buffer').Buffer;
/*</replacement>*/

Writable.WritableState = WritableState;


/*<replacement>*/
var util = require('core-util-is');
util.inherits = require('inherits');
/*</replacement>*/


var Stream = require('stream');

util.inherits(Writable, Stream);

function WriteReq(chunk, encoding, cb) {
  this.chunk = chunk;
  this.encoding = encoding;
  this.callback = cb;
}

function WritableState(options, stream) {
  options = options || {};

  // the point at which write() starts returning false
  // Note: 0 is a valid value, means that we always return false if
  // the entire buffer is not flushed immediately on write()
  var hwm = options.highWaterMark;
  this.highWaterMark = (hwm || hwm === 0) ? hwm : 16 * 1024;

  // object stream flag to indicate whether or not this stream
  // contains buffers or objects.
  this.objectMode = !!options.objectMode;

  // cast to ints.
  this.highWaterMark = ~~this.highWaterMark;

  this.needDrain = false;
  // at the start of calling end()
  this.ending = false;
  // when end() has been called, and returned
  this.ended = false;
  // when 'finish' is emitted
  this.finished = false;

  // should we decode strings into buffers before passing to _write?
  // this is here so that some node-core streams can optimize string
  // handling at a lower level.
  var noDecode = options.decodeStrings === false;
  this.decodeStrings = !noDecode;

  // Crypto is kind of old and crusty.  Historically, its default string
  // encoding is 'binary' so we have to make this configurable.
  // Everything else in the universe uses 'utf8', though.
  this.defaultEncoding = options.defaultEncoding || 'utf8';

  // not an actual buffer we keep track of, but a measurement
  // of how much we're waiting to get pushed to some underlying
  // socket or file.
  this.length = 0;

  // a flag to see when we're in the middle of a write.
  this.writing = false;

  // a flag to be able to tell if the onwrite cb is called immediately,
  // or on a later tick.  We set this to true at first, becuase any
  // actions that shouldn't happen until "later" should generally also
  // not happen before the first write call.
  this.sync = true;

  // a flag to know if we're processing previously buffered items, which
  // may call the _write() callback in the same tick, so that we don't
  // end up in an overlapped onwrite situation.
  this.bufferProcessing = false;

  // the callback that's passed to _write(chunk,cb)
  this.onwrite = function(er) {
    onwrite(stream, er);
  };

  // the callback that the user supplies to write(chunk,encoding,cb)
  this.writecb = null;

  // the amount that is being written when _write is called.
  this.writelen = 0;

  this.buffer = [];

  // True if the error was already emitted and should not be thrown again
  this.errorEmitted = false;
}

function Writable(options) {
  var Duplex = require('./_stream_duplex');

  // Writable ctor is applied to Duplexes, though they're not
  // instanceof Writable, they're instanceof Readable.
  if (!(this instanceof Writable) && !(this instanceof Duplex))
    return new Writable(options);

  this._writableState = new WritableState(options, this);

  // legacy.
  this.writable = true;

  Stream.call(this);
}

// Otherwise people can pipe Writable streams, which is just wrong.
Writable.prototype.pipe = function() {
  this.emit('error', new Error('Cannot pipe. Not readable.'));
};


function writeAfterEnd(stream, state, cb) {
  var er = new Error('write after end');
  // TODO: defer error events consistently everywhere, not just the cb
  stream.emit('error', er);
  process.nextTick(function() {
    cb(er);
  });
}

// If we get something that is not a buffer, string, null, or undefined,
// and we're not in objectMode, then that's an error.
// Otherwise stream chunks are all considered to be of length=1, and the
// watermarks determine how many objects to keep in the buffer, rather than
// how many bytes or characters.
function validChunk(stream, state, chunk, cb) {
  var valid = true;
  if (!Buffer.isBuffer(chunk) &&
      'string' !== typeof chunk &&
      chunk !== null &&
      chunk !== undefined &&
      !state.objectMode) {
    var er = new TypeError('Invalid non-string/buffer chunk');
    stream.emit('error', er);
    process.nextTick(function() {
      cb(er);
    });
    valid = false;
  }
  return valid;
}

Writable.prototype.write = function(chunk, encoding, cb) {
  var state = this._writableState;
  var ret = false;

  if (typeof encoding === 'function') {
    cb = encoding;
    encoding = null;
  }

  if (Buffer.isBuffer(chunk))
    encoding = 'buffer';
  else if (!encoding)
    encoding = state.defaultEncoding;

  if (typeof cb !== 'function')
    cb = function() {};

  if (state.ended)
    writeAfterEnd(this, state, cb);
  else if (validChunk(this, state, chunk, cb))
    ret = writeOrBuffer(this, state, chunk, encoding, cb);

  return ret;
};

function decodeChunk(state, chunk, encoding) {
  if (!state.objectMode &&
      state.decodeStrings !== false &&
      typeof chunk === 'string') {
    chunk = new Buffer(chunk, encoding);
  }
  return chunk;
}

// if we're already writing something, then just put this
// in the queue, and wait our turn.  Otherwise, call _write
// If we return false, then we need a drain event, so set that flag.
function writeOrBuffer(stream, state, chunk, encoding, cb) {
  chunk = decodeChunk(state, chunk, encoding);
  if (Buffer.isBuffer(chunk))
    encoding = 'buffer';
  var len = state.objectMode ? 1 : chunk.length;

  state.length += len;

  var ret = state.length < state.highWaterMark;
  // we must ensure that previous needDrain will not be reset to false.
  if (!ret)
    state.needDrain = true;

  if (state.writing)
    state.buffer.push(new WriteReq(chunk, encoding, cb));
  else
    doWrite(stream, state, len, chunk, encoding, cb);

  return ret;
}

function doWrite(stream, state, len, chunk, encoding, cb) {
  state.writelen = len;
  state.writecb = cb;
  state.writing = true;
  state.sync = true;
  stream._write(chunk, encoding, state.onwrite);
  state.sync = false;
}

function onwriteError(stream, state, sync, er, cb) {
  if (sync)
    process.nextTick(function() {
      cb(er);
    });
  else
    cb(er);

  stream._writableState.errorEmitted = true;
  stream.emit('error', er);
}

function onwriteStateUpdate(state) {
  state.writing = false;
  state.writecb = null;
  state.length -= state.writelen;
  state.writelen = 0;
}

function onwrite(stream, er) {
  var state = stream._writableState;
  var sync = state.sync;
  var cb = state.writecb;

  onwriteStateUpdate(state);

  if (er)
    onwriteError(stream, state, sync, er, cb);
  else {
    // Check if we're actually ready to finish, but don't emit yet
    var finished = needFinish(stream, state);

    if (!finished && !state.bufferProcessing && state.buffer.length)
      clearBuffer(stream, state);

    if (sync) {
      process.nextTick(function() {
        afterWrite(stream, state, finished, cb);
      });
    } else {
      afterWrite(stream, state, finished, cb);
    }
  }
}

function afterWrite(stream, state, finished, cb) {
  if (!finished)
    onwriteDrain(stream, state);
  cb();
  if (finished)
    finishMaybe(stream, state);
}

// Must force callback to be called on nextTick, so that we don't
// emit 'drain' before the write() consumer gets the 'false' return
// value, and has a chance to attach a 'drain' listener.
function onwriteDrain(stream, state) {
  if (state.length === 0 && state.needDrain) {
    state.needDrain = false;
    stream.emit('drain');
  }
}


// if there's something in the buffer waiting, then process it
function clearBuffer(stream, state) {
  state.bufferProcessing = true;

  for (var c = 0; c < state.buffer.length; c++) {
    var entry = state.buffer[c];
    var chunk = entry.chunk;
    var encoding = entry.encoding;
    var cb = entry.callback;
    var len = state.objectMode ? 1 : chunk.length;

    doWrite(stream, state, len, chunk, encoding, cb);

    // if we didn't call the onwrite immediately, then
    // it means that we need to wait until it does.
    // also, that means that the chunk and cb are currently
    // being processed, so move the buffer counter past them.
    if (state.writing) {
      c++;
      break;
    }
  }

  state.bufferProcessing = false;
  if (c < state.buffer.length)
    state.buffer = state.buffer.slice(c);
  else
    state.buffer.length = 0;
}

Writable.prototype._write = function(chunk, encoding, cb) {
  cb(new Error('not implemented'));
};

Writable.prototype.end = function(chunk, encoding, cb) {
  var state = this._writableState;

  if (typeof chunk === 'function') {
    cb = chunk;
    chunk = null;
    encoding = null;
  } else if (typeof encoding === 'function') {
    cb = encoding;
    encoding = null;
  }

  if (typeof chunk !== 'undefined' && chunk !== null)
    this.write(chunk, encoding);

  // ignore unnecessary end() calls.
  if (!state.ending && !state.finished)
    endWritable(this, state, cb);
};


function needFinish(stream, state) {
  return (state.ending &&
          state.length === 0 &&
          !state.finished &&
          !state.writing);
}

function finishMaybe(stream, state) {
  var need = needFinish(stream, state);
  if (need) {
    state.finished = true;
    stream.emit('finish');
  }
  return need;
}

function endWritable(stream, state, cb) {
  state.ending = true;
  finishMaybe(stream, state);
  if (cb) {
    if (state.finished)
      process.nextTick(cb);
    else
      stream.once('finish', cb);
  }
  state.ended = true;
}

}).call(this,require("/home/noah/Projects/shimi_ima/node_modules/browserify/node_modules/insert-module-globals/node_modules/process/browser.js"))
},{"./_stream_duplex":41,"/home/noah/Projects/shimi_ima/node_modules/browserify/node_modules/insert-module-globals/node_modules/process/browser.js":12,"buffer":1,"core-util-is":46,"inherits":47,"stream":14}],46:[function(require,module,exports){
(function (Buffer){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

// NOTE: These type checking functions intentionally don't use `instanceof`
// because it is fragile and can be easily faked with `Object.create()`.
function isArray(ar) {
  return Array.isArray(ar);
}
exports.isArray = isArray;

function isBoolean(arg) {
  return typeof arg === 'boolean';
}
exports.isBoolean = isBoolean;

function isNull(arg) {
  return arg === null;
}
exports.isNull = isNull;

function isNullOrUndefined(arg) {
  return arg == null;
}
exports.isNullOrUndefined = isNullOrUndefined;

function isNumber(arg) {
  return typeof arg === 'number';
}
exports.isNumber = isNumber;

function isString(arg) {
  return typeof arg === 'string';
}
exports.isString = isString;

function isSymbol(arg) {
  return typeof arg === 'symbol';
}
exports.isSymbol = isSymbol;

function isUndefined(arg) {
  return arg === void 0;
}
exports.isUndefined = isUndefined;

function isRegExp(re) {
  return isObject(re) && objectToString(re) === '[object RegExp]';
}
exports.isRegExp = isRegExp;

function isObject(arg) {
  return typeof arg === 'object' && arg !== null;
}
exports.isObject = isObject;

function isDate(d) {
  return isObject(d) && objectToString(d) === '[object Date]';
}
exports.isDate = isDate;

function isError(e) {
  return isObject(e) &&
      (objectToString(e) === '[object Error]' || e instanceof Error);
}
exports.isError = isError;

function isFunction(arg) {
  return typeof arg === 'function';
}
exports.isFunction = isFunction;

function isPrimitive(arg) {
  return arg === null ||
         typeof arg === 'boolean' ||
         typeof arg === 'number' ||
         typeof arg === 'string' ||
         typeof arg === 'symbol' ||  // ES6 symbol
         typeof arg === 'undefined';
}
exports.isPrimitive = isPrimitive;

function isBuffer(arg) {
  return Buffer.isBuffer(arg);
}
exports.isBuffer = isBuffer;

function objectToString(o) {
  return Object.prototype.toString.call(o);
}
}).call(this,require("buffer").Buffer)
},{"buffer":1}],47:[function(require,module,exports){
module.exports=require(11)
},{}],48:[function(require,module,exports){
module.exports = Array.isArray || function (arr) {
  return Object.prototype.toString.call(arr) == '[object Array]';
};

},{}],49:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

var Buffer = require('buffer').Buffer;

var isBufferEncoding = Buffer.isEncoding
  || function(encoding) {
       switch (encoding && encoding.toLowerCase()) {
         case 'hex': case 'utf8': case 'utf-8': case 'ascii': case 'binary': case 'base64': case 'ucs2': case 'ucs-2': case 'utf16le': case 'utf-16le': case 'raw': return true;
         default: return false;
       }
     }


function assertEncoding(encoding) {
  if (encoding && !isBufferEncoding(encoding)) {
    throw new Error('Unknown encoding: ' + encoding);
  }
}

var StringDecoder = exports.StringDecoder = function(encoding) {
  this.encoding = (encoding || 'utf8').toLowerCase().replace(/[-_]/, '');
  assertEncoding(encoding);
  switch (this.encoding) {
    case 'utf8':
      // CESU-8 represents each of Surrogate Pair by 3-bytes
      this.surrogateSize = 3;
      break;
    case 'ucs2':
    case 'utf16le':
      // UTF-16 represents each of Surrogate Pair by 2-bytes
      this.surrogateSize = 2;
      this.detectIncompleteChar = utf16DetectIncompleteChar;
      break;
    case 'base64':
      // Base-64 stores 3 bytes in 4 chars, and pads the remainder.
      this.surrogateSize = 3;
      this.detectIncompleteChar = base64DetectIncompleteChar;
      break;
    default:
      this.write = passThroughWrite;
      return;
  }

  this.charBuffer = new Buffer(6);
  this.charReceived = 0;
  this.charLength = 0;
};


StringDecoder.prototype.write = function(buffer) {
  var charStr = '';
  var offset = 0;

  // if our last write ended with an incomplete multibyte character
  while (this.charLength) {
    // determine how many remaining bytes this buffer has to offer for this char
    var i = (buffer.length >= this.charLength - this.charReceived) ?
                this.charLength - this.charReceived :
                buffer.length;

    // add the new bytes to the char buffer
    buffer.copy(this.charBuffer, this.charReceived, offset, i);
    this.charReceived += (i - offset);
    offset = i;

    if (this.charReceived < this.charLength) {
      // still not enough chars in this buffer? wait for more ...
      return '';
    }

    // get the character that was split
    charStr = this.charBuffer.slice(0, this.charLength).toString(this.encoding);

    // lead surrogate (D800-DBFF) is also the incomplete character
    var charCode = charStr.charCodeAt(charStr.length - 1);
    if (charCode >= 0xD800 && charCode <= 0xDBFF) {
      this.charLength += this.surrogateSize;
      charStr = '';
      continue;
    }
    this.charReceived = this.charLength = 0;

    // if there are no more bytes in this buffer, just emit our char
    if (i == buffer.length) return charStr;

    // otherwise cut off the characters end from the beginning of this buffer
    buffer = buffer.slice(i, buffer.length);
    break;
  }

  var lenIncomplete = this.detectIncompleteChar(buffer);

  var end = buffer.length;
  if (this.charLength) {
    // buffer the incomplete character bytes we got
    buffer.copy(this.charBuffer, 0, buffer.length - lenIncomplete, end);
    this.charReceived = lenIncomplete;
    end -= lenIncomplete;
  }

  charStr += buffer.toString(this.encoding, 0, end);

  var end = charStr.length - 1;
  var charCode = charStr.charCodeAt(end);
  // lead surrogate (D800-DBFF) is also the incomplete character
  if (charCode >= 0xD800 && charCode <= 0xDBFF) {
    var size = this.surrogateSize;
    this.charLength += size;
    this.charReceived += size;
    this.charBuffer.copy(this.charBuffer, size, 0, size);
    this.charBuffer.write(charStr.charAt(charStr.length - 1), this.encoding);
    return charStr.substring(0, end);
  }

  // or just emit the charStr
  return charStr;
};

StringDecoder.prototype.detectIncompleteChar = function(buffer) {
  // determine how many bytes we have to check at the end of this buffer
  var i = (buffer.length >= 3) ? 3 : buffer.length;

  // Figure out if one of the last i bytes of our buffer announces an
  // incomplete char.
  for (; i > 0; i--) {
    var c = buffer[buffer.length - i];

    // See http://en.wikipedia.org/wiki/UTF-8#Description

    // 110XXXXX
    if (i == 1 && c >> 5 == 0x06) {
      this.charLength = 2;
      break;
    }

    // 1110XXXX
    if (i <= 2 && c >> 4 == 0x0E) {
      this.charLength = 3;
      break;
    }

    // 11110XXX
    if (i <= 3 && c >> 3 == 0x1E) {
      this.charLength = 4;
      break;
    }
  }

  return i;
};

StringDecoder.prototype.end = function(buffer) {
  var res = '';
  if (buffer && buffer.length)
    res = this.write(buffer);

  if (this.charReceived) {
    var cr = this.charReceived;
    var buf = this.charBuffer;
    var enc = this.encoding;
    res += buf.slice(0, cr).toString(enc);
  }

  return res;
};

function passThroughWrite(buffer) {
  return buffer.toString(this.encoding);
}

function utf16DetectIncompleteChar(buffer) {
  var incomplete = this.charReceived = buffer.length % 2;
  this.charLength = incomplete ? 2 : 0;
  return incomplete;
}

function base64DetectIncompleteChar(buffer) {
  var incomplete = this.charReceived = buffer.length % 3;
  this.charLength = incomplete ? 3 : 0;
  return incomplete;
}

},{"buffer":1}],50:[function(require,module,exports){
exports = module.exports = require('./lib/_stream_readable.js');
exports.Readable = exports;
exports.Writable = require('./lib/_stream_writable.js');
exports.Duplex = require('./lib/_stream_duplex.js');
exports.Transform = require('./lib/_stream_transform.js');
exports.PassThrough = require('./lib/_stream_passthrough.js');

},{"./lib/_stream_duplex.js":41,"./lib/_stream_passthrough.js":42,"./lib/_stream_readable.js":43,"./lib/_stream_transform.js":44,"./lib/_stream_writable.js":45}],51:[function(require,module,exports){
(function (Buffer){
//     uuid.js
//
//     Copyright (c) 2010-2012 Robert Kieffer
//     MIT License - http://opensource.org/licenses/mit-license.php

(function() {
  var _global = this;

  // Unique ID creation requires a high quality random # generator.  We feature
  // detect to determine the best RNG source, normalizing to a function that
  // returns 128-bits of randomness, since that's what's usually required
  var _rng;

  // Node.js crypto-based RNG - http://nodejs.org/docs/v0.6.2/api/crypto.html
  //
  // Moderately fast, high quality
  if (typeof(require) == 'function') {
    try {
      var _rb = require('crypto').randomBytes;
      _rng = _rb && function() {return _rb(16);};
    } catch(e) {}
  }

  if (!_rng && _global.crypto && crypto.getRandomValues) {
    // WHATWG crypto-based RNG - http://wiki.whatwg.org/wiki/Crypto
    //
    // Moderately fast, high quality
    var _rnds8 = new Uint8Array(16);
    _rng = function whatwgRNG() {
      crypto.getRandomValues(_rnds8);
      return _rnds8;
    };
  }

  if (!_rng) {
    // Math.random()-based (RNG)
    //
    // If all else fails, use Math.random().  It's fast, but is of unspecified
    // quality.
    var  _rnds = new Array(16);
    _rng = function() {
      for (var i = 0, r; i < 16; i++) {
        if ((i & 0x03) === 0) r = Math.random() * 0x100000000;
        _rnds[i] = r >>> ((i & 0x03) << 3) & 0xff;
      }

      return _rnds;
    };
  }

  // Buffer class to use
  var BufferClass = typeof(Buffer) == 'function' ? Buffer : Array;

  // Maps for number <-> hex string conversion
  var _byteToHex = [];
  var _hexToByte = {};
  for (var i = 0; i < 256; i++) {
    _byteToHex[i] = (i + 0x100).toString(16).substr(1);
    _hexToByte[_byteToHex[i]] = i;
  }

  // **`parse()` - Parse a UUID into it's component bytes**
  function parse(s, buf, offset) {
    var i = (buf && offset) || 0, ii = 0;

    buf = buf || [];
    s.toLowerCase().replace(/[0-9a-f]{2}/g, function(oct) {
      if (ii < 16) { // Don't overflow!
        buf[i + ii++] = _hexToByte[oct];
      }
    });

    // Zero out remaining bytes if string was short
    while (ii < 16) {
      buf[i + ii++] = 0;
    }

    return buf;
  }

  // **`unparse()` - Convert UUID byte array (ala parse()) into a string**
  function unparse(buf, offset) {
    var i = offset || 0, bth = _byteToHex;
    return  bth[buf[i++]] + bth[buf[i++]] +
            bth[buf[i++]] + bth[buf[i++]] + '-' +
            bth[buf[i++]] + bth[buf[i++]] + '-' +
            bth[buf[i++]] + bth[buf[i++]] + '-' +
            bth[buf[i++]] + bth[buf[i++]] + '-' +
            bth[buf[i++]] + bth[buf[i++]] +
            bth[buf[i++]] + bth[buf[i++]] +
            bth[buf[i++]] + bth[buf[i++]];
  }

  // **`v1()` - Generate time-based UUID**
  //
  // Inspired by https://github.com/LiosK/UUID.js
  // and http://docs.python.org/library/uuid.html

  // random #'s we need to init node and clockseq
  var _seedBytes = _rng();

  // Per 4.5, create and 48-bit node id, (47 random bits + multicast bit = 1)
  var _nodeId = [
    _seedBytes[0] | 0x01,
    _seedBytes[1], _seedBytes[2], _seedBytes[3], _seedBytes[4], _seedBytes[5]
  ];

  // Per 4.2.2, randomize (14 bit) clockseq
  var _clockseq = (_seedBytes[6] << 8 | _seedBytes[7]) & 0x3fff;

  // Previous uuid creation time
  var _lastMSecs = 0, _lastNSecs = 0;

  // See https://github.com/broofa/node-uuid for API details
  function v1(options, buf, offset) {
    var i = buf && offset || 0;
    var b = buf || [];

    options = options || {};

    var clockseq = options.clockseq != null ? options.clockseq : _clockseq;

    // UUID timestamps are 100 nano-second units since the Gregorian epoch,
    // (1582-10-15 00:00).  JSNumbers aren't precise enough for this, so
    // time is handled internally as 'msecs' (integer milliseconds) and 'nsecs'
    // (100-nanoseconds offset from msecs) since unix epoch, 1970-01-01 00:00.
    var msecs = options.msecs != null ? options.msecs : new Date().getTime();

    // Per 4.2.1.2, use count of uuid's generated during the current clock
    // cycle to simulate higher resolution clock
    var nsecs = options.nsecs != null ? options.nsecs : _lastNSecs + 1;

    // Time since last uuid creation (in msecs)
    var dt = (msecs - _lastMSecs) + (nsecs - _lastNSecs)/10000;

    // Per 4.2.1.2, Bump clockseq on clock regression
    if (dt < 0 && options.clockseq == null) {
      clockseq = clockseq + 1 & 0x3fff;
    }

    // Reset nsecs if clock regresses (new clockseq) or we've moved onto a new
    // time interval
    if ((dt < 0 || msecs > _lastMSecs) && options.nsecs == null) {
      nsecs = 0;
    }

    // Per 4.2.1.2 Throw error if too many uuids are requested
    if (nsecs >= 10000) {
      throw new Error('uuid.v1(): Can\'t create more than 10M uuids/sec');
    }

    _lastMSecs = msecs;
    _lastNSecs = nsecs;
    _clockseq = clockseq;

    // Per 4.1.4 - Convert from unix epoch to Gregorian epoch
    msecs += 12219292800000;

    // `time_low`
    var tl = ((msecs & 0xfffffff) * 10000 + nsecs) % 0x100000000;
    b[i++] = tl >>> 24 & 0xff;
    b[i++] = tl >>> 16 & 0xff;
    b[i++] = tl >>> 8 & 0xff;
    b[i++] = tl & 0xff;

    // `time_mid`
    var tmh = (msecs / 0x100000000 * 10000) & 0xfffffff;
    b[i++] = tmh >>> 8 & 0xff;
    b[i++] = tmh & 0xff;

    // `time_high_and_version`
    b[i++] = tmh >>> 24 & 0xf | 0x10; // include version
    b[i++] = tmh >>> 16 & 0xff;

    // `clock_seq_hi_and_reserved` (Per 4.2.2 - include variant)
    b[i++] = clockseq >>> 8 | 0x80;

    // `clock_seq_low`
    b[i++] = clockseq & 0xff;

    // `node`
    var node = options.node || _nodeId;
    for (var n = 0; n < 6; n++) {
      b[i + n] = node[n];
    }

    return buf ? buf : unparse(b);
  }

  // **`v4()` - Generate random UUID**

  // See https://github.com/broofa/node-uuid for API details
  function v4(options, buf, offset) {
    // Deprecated - 'format' argument, as supported in v1.2
    var i = buf && offset || 0;

    if (typeof(options) == 'string') {
      buf = options == 'binary' ? new BufferClass(16) : null;
      options = null;
    }
    options = options || {};

    var rnds = options.random || (options.rng || _rng)();

    // Per 4.4, set bits for version and `clock_seq_hi_and_reserved`
    rnds[6] = (rnds[6] & 0x0f) | 0x40;
    rnds[8] = (rnds[8] & 0x3f) | 0x80;

    // Copy bytes to buffer, if provided
    if (buf) {
      for (var ii = 0; ii < 16; ii++) {
        buf[i + ii] = rnds[ii];
      }
    }

    return buf || unparse(rnds);
  }

  // Export public API
  var uuid = v4;
  uuid.v1 = v1;
  uuid.v4 = v4;
  uuid.parse = parse;
  uuid.unparse = unparse;
  uuid.BufferClass = BufferClass;

  if (typeof define === 'function' && define.amd) {
    // Publish as AMD module
    define(function() {return uuid;});
  } else if (typeof(module) != 'undefined' && module.exports) {
    // Publish as node.js module
    module.exports = uuid;
  } else {
    // Publish as global (in browsers)
    var _previousRoot = _global.uuid;

    // **`noConflict()` - (browser only) to reset global 'uuid' var**
    uuid.noConflict = function() {
      _global.uuid = _previousRoot;
      return uuid;
    };

    _global.uuid = uuid;
  }
}).call(this);

}).call(this,require("buffer").Buffer)
},{"buffer":1,"crypto":5}],52:[function(require,module,exports){
var Hogan = require('hogan.js');
var t = {
  'changelog-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");if(_.s(_.f("doc",c,p,1),c,p,0,8,777,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<tr class=\"change-header\" id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,73,188,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    <a");_.b("\n" + i);_.b("      href=\"#");_.b(_.v(_.f("document_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      class=\"view-document-link\">");_.b("\n" + i);_.b("      ");if(_.s(_.f("head_values",c,p,1),c,p,0,298,305,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.v(_.d(".",c,p,0)));});c.pop();}_.b("\n" + i);_.b("    </a>");_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("change_type",c,p,0)));_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("user",c,p,0)));_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("timestamp",c,p,0)));_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("</tr>");_.b("\n" + i);if(_.s(_.f("changes",c,p,1),c,p,0,459,764,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <tr class=\"change-change\">");_.b("\n" + i);_.b("    <th>");_.b("\n" + i);_.b("      ");_.b(_.v(_.f("fieldsetLabel",c,p,0)));_.b(": ");_.b(_.v(_.f("fieldLabel",c,p,0)));_.b("\n" + i);_.b("    </th>");_.b("\n" + i);_.b("    <td colspan=3>");_.b("\n" + i);if(!_.s(_.f("originalValue",c,p,1),c,p,1,0,0,"")){_.b("      <b>Ø</b>");_.b("\n");};_.b("      ");_.b(_.v(_.f("originalValue",c,p,0)));_.b("\n" + i);_.b("      →");_.b("\n" + i);if(!_.s(_.f("newValue",c,p,1),c,p,1,0,0,"")){_.b("      <b>Ø</b>");_.b("\n");};_.b("      ");_.b(_.v(_.f("newValue",c,p,0)));_.b("\n" + i);_.b("    </td>");_.b("\n" + i);_.b("  </tr>");_.b("\n");});c.pop();}});c.pop();}return _.fl();;}),
  'charseqs-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,42,157,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("key",c,p,0)));_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("value",c,p,0)));_.b("\n" + i);_.b("   </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'config-maintenance' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<div id=\"maintenance\">");_.b("\n" + i);_.b("  <h3>Upgrade Project</h3>");_.b("\n" + i);_.b("\n" + i);_.b("  <p>");_.b("\n" + i);_.b("    Clicking the button below will initiate an upgrade of the project");_.b("\n" + i);_.b("    core design document to the latest version available on your");_.b("\n" + i);_.b("    system.");_.b("\n" + i);_.b("  </p>");_.b("\n" + i);_.b("  <p>");_.b("\n" + i);_.b("    Be aware that this may cause significant slowness on your system");_.b("\n" + i);_.b("    while view indexes are rebuilt.");_.b("\n" + i);_.b("  </p>");_.b("\n" + i);_.b("\n" + i);_.b("  <a id=\"maintenance-upgrade-button\" class=\"maintenance-upgrade-button link-button\">Upgrade</a>");_.b("\n" + i);_.b("</div>");_.b("\n");return _.fl();;}),
  'doctypes-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,42,157,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    <a href=\"#");_.b(_.v(_.f("id",c,p,0)));_.b("\" class=\"edit-doctype-link\">");_.b(_.v(_.f("key",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("value",c,p,0)));_.b("\n" + i);_.b("   </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'document-edit' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<h2 class=\"header\">Edit</h2>");_.b("\n" + i);_.b("\n" + i);if(_.s(_.f("has_rows",c,p,1),c,p,0,43,1862,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <div id=\"edit-document-form\" class=\"ui-widget ui-corner-all\">");_.b("\n" + i);_.b("  ");_.b("\n" + i);_.b("    <div id=\"edit-tabs\">");_.b("\n" + i);_.b("      <div id=\"tabs-container\">");_.b("\n" + i);_.b("        <ul id=\"tab-list\">");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,219,338,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          <li>");_.b("\n" + i);_.b("            <a href=\"#");_.b(_.v(_.f("_id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("              ");_.b(_.v(_.f("label",c,p,0)));_.b("\n" + i);_.b("            </a>");_.b("\n" + i);_.b("          </li>");_.b("\n");});c.pop();}_.b("        </ul>");_.b("\n" + i);_.b("      </div>");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,400,1448,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <fieldset");_.b("\n" + i);_.b("          id=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("          class=\"ui-widget ui-widget-content ui-corner-all fieldset\"");_.b("\n" + i);_.b("          data-group-id=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-field-fieldset=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("          data-field-project=\"project-");_.b(_.v(_.f("project_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-field-doctype=\"");_.b(_.v(_.f("doctype",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-fieldset-fieldset=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("          data-fieldset-multiple=\"");_.b(_.v(_.f("multiple",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-fieldset-collapse=\"");_.b(_.v(_.f("collapse",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-fieldset-name=\"");_.b(_.v(_.f("name",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-fieldset-label=\"");_.b(_.v(_.f("label",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-fieldset-order=\"");_.b(_.v(_.f("order",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-fieldset-project=\"project-");_.b(_.v(_.f("project_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-fieldset-doctype=\"");_.b(_.v(_.f("doctype_id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("          <p>");_.b(_.v(_.f("description",c,p,0)));_.b("</p>");_.b("\n" + i);_.b("          <div ");_.b("\n" + i);_.b("            id=\"container-");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("            class=\"fieldset-container\"");_.b("\n" + i);_.b("            data-group-id=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\"></div>");_.b("\n" + i);if(_.s(_.f("multiple",c,p,1),c,p,0,1279,1408,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("              <a ");_.b("\n" + i);_.b("                class=\"add-button link-button\" ");_.b("\n" + i);_.b("                data-group-id=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\">Add</a>");_.b("\n");});c.pop();}_.b("        </fieldset>");_.b("\n");});c.pop();}_.b("    </div>");_.b("\n" + i);_.b("    <div id=\"submit-button-area\">");_.b("\n" + i);_.b("      <a id=\"clear-document-button\" class=\"clear-button link-button\">Clear Form</a>");_.b("\n" + i);_.b("      <a data-group-id=\"all-document-container\" id=\"create-document-button\" class=\"create-button link-button\">Create as New</a>");_.b("\n" + i);_.b("      <a data-group-id=\"all-document-container\" id=\"save-document-button\" class=\"save-button link-button hidden\">Save</a>");_.b("\n" + i);_.b("    </div>");_.b("\n" + i);_.b("  </div>");_.b("\n");});c.pop();}if(!_.s(_.f("has_rows",c,p,1),c,p,1,0,0,"")){_.b("<p>");_.b("\n" + i);_.b("  You must add fields and fieldsets before you can create a document of this type.");_.b("\n" + i);_.b("</p>");_.b("\n");};return _.fl();;}),
  'document-search-results' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr>");_.b("\n" + i);_.b("  <th>");_.b("\n" + i);_.b("    <a href=\"#");_.b(_.v(_.f("id",c,p,0)));_.b("\" class=\"view-document-link\">");_.b(_.v(_.f("key",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td class=\"search-result-context\">");_.b("\n" + i);_.b("    <a href=\"#");_.b(_.v(_.f("id",c,p,0)));_.b("\" class=\"view-document-link\">");_.b(_.v(_.f("value",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'document-search' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");if(_.s(_.f("are_results",c,p,1),c,p,0,16,1213,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <div class=\"total-rows-info\">");_.b("\n" + i);_.b("    <b>Total</b>: ");_.b(_.v(_.f("total_rows",c,p,0)));_.b("\n" + i);_.b("  </div>");_.b("\n" + i);_.b("  <div id=\"save-search-results\">");_.b("\n" + i);_.b("    <a href=\"#\">(Save Selected)</a>");_.b("\n" + i);_.b("  </div>");_.b("\n" + i);if(_.s(_.f("index_listing",c,p,1),c,p,0,191,477,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <div class=\"search-results\">");_.b("\n" + i);_.b("      <input type=\"checkbox\" class=\"select-results\" name=\"select-results\" />");_.b("\n" + i);_.b("      <label for=\"select-results\">Select Results</label>");_.b("\n" + i);_.b("      <table>");_.b("\n" + i);if(_.s(_.f("rows",c,p,1),c,p,0,390,439,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("document-search-results",c,p,"          "));});c.pop();}_.b("      </table>");_.b("\n" + i);_.b("    </div>");_.b("\n");});c.pop();}if(!_.s(_.f("index_listing",c,p,1),c,p,1,0,0,"")){if(_.s(_.f("rows",c,p,1),c,p,0,530,1182,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <h5 class=\"search-result-field-id toggler\"");_.b("\n" + i);_.b("        data-field-field=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("        data-target=\"results-for-field-");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("        title=\"Click to display\">");_.b("\n" + i);_.b("      <a href=\"#\" title=\"Double click to add as search option\">");_.b("\n" + i);_.b("        ");_.b(_.v(_.f("id",c,p,0)));_.b("\n" + i);_.b("      </a> ");_.b("\n" + i);_.b("      (");_.b(_.v(_.f("total_rows",c,p,0)));_.b(")");_.b("\n" + i);_.b("    </h5>");_.b("\n" + i);_.b("    ");_.b("\n" + i);_.b("    <div class=\"search-results\"");_.b("\n" + i);_.b("         id=\"results-for-field-");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("      <input type=\"checkbox\" class=\"select-results\" name=\"select-results-");_.b(_.v(_.d("field.id",c,p,0)));_.b("\" />");_.b("\n" + i);_.b("      <label for=\"select-results-");_.b(_.v(_.f("id",c,p,0)));_.b("\">Select Results</label>");_.b("\n" + i);_.b("      <table>");_.b("\n" + i);if(_.s(_.f("rows",c,p,1),c,p,0,1093,1142,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("document-search-results",c,p,"          "));});c.pop();}_.b("      </table>");_.b("\n" + i);_.b("    </div>");_.b("\n");});c.pop();}};});c.pop();}if(!_.s(_.f("are_results",c,p,1),c,p,1,0,0,"")){_.b("  <em>No Results</em>");_.b("\n");};return _.fl();;}),
  'document-view-field' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<li ");_.b("\n" + i);_.b("  class=\"field-view ");_.b("\n" + i);_.b("    ");if(_.s(_.f("changed",c,p,1),c,p,0,42,49,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("changed");});c.pop();}_.b("\"");_.b("\n" + i);_.b("  data-field-field=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);if(_.s(_.f("instance",c,p,1),c,p,0,108,150,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  data-field-instance=\"");_.b(_.v(_.f("instance",c,p,0)));_.b("\"");_.b("\n");});c.pop();}_.b("  data-field-value=\"");_.b(_.v(_.f("json_value",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <b>");_.b(_.v(_.f("label",c,p,0)));_.b("</b>");if(_.s(_.f("changed",c,p,1),c,p,0,235,343,"{{ }}")){_.rs(c,p,function(c,p,_){if(!_.s(_.f("newfield",c,p,1),c,p,1,0,0,"")){_.b("<span class=\"small-control view-field-change\" title=\"");_.b(_.v(_.f("originalValue",c,p,0)));_.b("\">→</span>");};});c.pop();}_.b(":");_.b("\n" + i);if(_.s(_.f("is_textarea",c,p,1),c,p,0,375,426,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <span class=\"retain-white\">");_.b(_.v(_.f("value",c,p,0)));_.b("</span>");_.b("\n");});c.pop();}if(!_.s(_.f("is_textarea",c,p,1),c,p,1,0,0,"")){_.b("    ");_.b(_.v(_.f("value",c,p,0)));_.b("\n");};_.b("</li>");_.b("\n");return _.fl();;}),
  'document-view-tree' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");if(_.s(_.f("previous_revision",c,p,1),c,p,0,22,76,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <div id=\"revision-message\">Previous Revision</div>");_.b("\n");});c.pop();}_.b("\n" + i);if(_.s(_.f("deleted_",c,p,1),c,p,0,113,163,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <div id=\"deleted-message\"><b>Deleted</b></div>");_.b("\n");});c.pop();}_.b("\n" + i);_.b("<ul>");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,197,975,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <li");_.b("\n" + i);_.b("    class=\"fieldset-view");_.b("\n" + i);_.b("      ");if(_.s(_.f("collapse",c,p,1),c,p,0,248,257,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("collapsed");});c.pop();}_.b("\n" + i);_.b("      ");if(_.s(_.f("altered",c,p,1),c,p,0,289,296,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("changed");});c.pop();}_.b("\"");_.b("\n" + i);_.b("    data-fieldset-fieldset=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("    data-group-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("    <b>");_.b(_.v(_.f("label",c,p,0)));_.b("</b>");if(_.s(_.f("addition",c,p,1),c,p,0,414,468,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<span title=\"fieldset added\" class=\"addition\">+</span>");});c.pop();}if(_.s(_.f("removal",c,p,1),c,p,0,493,548,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<span title=\"fieldset removed\" class=\"removal\">−</span>");});c.pop();}_.b(":");_.b("\n" + i);if(_.s(_.f("multiple",c,p,1),c,p,0,579,818,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <ol>");_.b("\n" + i);if(_.s(_.f("multifields",c,p,1),c,p,0,613,785,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <li>");_.b("\n" + i);_.b("          <ul class=\"multifield\">");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,684,737,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("document-view-field",c,p,"              "));});c.pop();}_.b("          </ul>");_.b("\n" + i);_.b("        </li>");_.b("\n");});c.pop();}_.b("      </ol>");_.b("\n");});c.pop();}if(!_.s(_.f("multiple",c,p,1),c,p,1,0,0,"")){_.b("      <ul>");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,880,925,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("document-view-field",c,p,"          "));});c.pop();}_.b("      </ul>");_.b("\n");};_.b("  </li>");_.b("\n");});c.pop();}_.b("</ul>");_.b("\n" + i);_.b("\n" + i);_.b("<div class=\"timestamps\">");_.b("\n" + i);_.b("  <dl>");_.b("\n" + i);_.b("    <dt>Created At</dt><dd class=\"timestamp\">");_.b(_.v(_.f("created_at_",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("    <dt>Created By</dt><dd>");_.b(_.v(_.f("created_by_",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("    <dt>Updated At</dt><dd class=\"timestamp\">");_.b(_.v(_.f("updated_at_",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("    <dt>Updated By</dt><dd>");_.b(_.v(_.f("updated_by_",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("    <dt>ID</dt><dd>");_.b(_.v(_.f("_id",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("  </dl>");_.b("\n" + i);_.b("</div>");_.b("\n");return _.fl();;}),
  'document-view' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");if(_.s(_.f("doctype_info",c,p,1),c,p,0,17,187,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <h2 class=\"header\">View</h2>");_.b("\n" + i);_.b("\n" + i);_.b("  <form id=\"view-jump\">");_.b("\n" + i);_.b("    <label for=\"view-jump-id\">Id</label>");_.b("\n" + i);_.b("    <input type=\"text\" id=\"view-jump-id\" name=\"view-jump-id\">");_.b("\n" + i);_.b("  </form>");_.b("\n");});c.pop();}_.b("\n" + i);_.b("<div id=\"document-view-info\"");_.b("\n" + i);_.b("     data-document-deleted=\"");_.b(_.v(_.f("deleted_",c,p,0)));_.b("\"");_.b("\n" + i);_.b("     data-document-document=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("     data-document-rev=\"");_.b(_.v(_.f("_rev",c,p,0)));_.b("\"></div>");_.b("\n" + i);_.b("\n" + i);_.b("<a id=\"document-restore-button\"");_.b("\n" + i);_.b("   data-group-id=\"document-view-info\"");_.b("\n" + i);_.b("   class=\"link-button hidden\">Restore</a>");_.b("\n" + i);_.b("\n" + i);_.b("<a id=\"document-edit-button\"");_.b("\n" + i);_.b("   data-group-id=\"document-view-info\"");_.b("\n" + i);_.b("   class=\"link-button\">Edit</a>");_.b("\n" + i);_.b("\n" + i);_.b("<a id=\"document-delete-button\"");_.b("\n" + i);_.b("   data-group-id=\"document-view-info\"");_.b("\n" + i);_.b("   class=\"link-button\">Delete</a>");_.b("\n" + i);_.b("\n" + i);_.b("<nav id=\"history\">");_.b("\n" + i);if(_.s(_.f("revs_info",c,p,1),c,p,0,716,991,"{{ }}")){_.rs(c,p,function(c,p,_){if(_.s(_.f("status",c,p,1),c,p,0,732,975,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <a href=\"#");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("         class=\"revision-link\"");_.b("\n" + i);_.b("         data-group-id=\"document-view-info\"");_.b("\n" + i);if(_.s(_.f("first",c,p,1),c,p,0,854,900,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("         id=\"current-revision-link\"");_.b("\n");});c.pop();}_.b("         data-document-oldrev=\"");_.b(_.v(_.f("rev",c,p,0)));_.b("\">");_.b(_.v(_.f("count",c,p,0)));_.b("</a>");_.b("\n");});c.pop();}});c.pop();}_.b("</nav>");_.b("\n" + i);_.b("\n" + i);_.b("<div id=\"document-view-tree\">");_.b("\n" + i);_.b(_.rp("document-view-tree",c,p,"  "));_.b("</div>");_.b("\n");return _.fl();;}),
  'fields' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");if(_.s(_.f("fields",c,p,1),c,p,0,11,5710,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <div ");_.b("\n" + i);_.b("    class=\"field-container\" ");_.b("\n" + i);_.b("    data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("    <label for=\"");_.b(_.v(_.f("name",c,p,0)));_.b("\">");_.b("\n" + i);_.b("      <span class=\"label-text\">");_.b(_.v(_.f("label",c,p,0)));_.b("</span>");_.b("\n" + i);_.b("      <span ");_.b("\n" + i);_.b("        class=\"ui-icon ui-icon-help\" ");_.b("\n" + i);_.b("        title=\"");if(_.s(_.f("date",c,p,1),c,p,0,237,264,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("Format date as: yyyy-mm-dd.");});c.pop();}_.b(" ");_.b(_.v(_.f("description",c,p,0)));_.b("\"></span>");_.b("\n" + i);_.b("    </label>");_.b("\n" + i);if(_.s(_.f("text",c,p,1),c,p,0,327,602,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <input ");_.b("\n" + i);_.b("      class=\"field text ui-widget ui-corner-all\" ");_.b("\n" + i);_.b("      type=\"text\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,435,511,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      value=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}if(!_.s(_.f("default_exists",c,p,1),c,p,1,0,0,"")){_.b("      value=\"\"");_.b("\n");};});c.pop();}if(_.s(_.f("integer",c,p,1),c,p,0,628,905,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <input ");_.b("\n" + i);_.b("      class=\"field number ui-widget ui-corner-all\" ");_.b("\n" + i);_.b("      type=\"text\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,738,814,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      value=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}if(!_.s(_.f("default_exists",c,p,1),c,p,1,0,0,"")){_.b("      value=\"\"");_.b("\n");};});c.pop();}if(_.s(_.f("rational",c,p,1),c,p,0,935,1212,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <input ");_.b("\n" + i);_.b("      class=\"field number ui-widget ui-corner-all\" ");_.b("\n" + i);_.b("      type=\"text\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,1045,1121,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      value=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}if(!_.s(_.f("default_exists",c,p,1),c,p,1,0,0,"")){_.b("      value=\"\"");_.b("\n");};});c.pop();}if(_.s(_.f("date",c,p,1),c,p,0,1239,1525,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <input ");_.b("\n" + i);_.b("      class=\"field date field-text ui-widget ui-corner-all\" ");_.b("\n" + i);_.b("      type=\"date\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,1358,1434,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      value=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}if(!_.s(_.f("default_exists",c,p,1),c,p,1,0,0,"")){_.b("      value=\"\"");_.b("\n");};});c.pop();}if(_.s(_.f("boolean",c,p,1),c,p,0,1551,1711,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <input");_.b("\n" + i);_.b("      class=\"boolean field ui-widget ui-corner-all\"");_.b("\n" + i);_.b("      type=\"checkbox\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,1664,1687,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        checked");_.b("\n");});c.pop();}});c.pop();}if(_.s(_.f("openboolean",c,p,1),c,p,0,1744,1924,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <select ");_.b("\n" + i);_.b("        class=\"field open-boolean ui-widget ui-corner-all\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,1846,1900,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}});c.pop();}if(_.s(_.f("select",c,p,1),c,p,0,1956,2130,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <select ");_.b("\n" + i);_.b("        class=\"field select ui-widget ui-corner-all\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,2052,2106,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}});c.pop();}if(_.s(_.f("docselect",c,p,1),c,p,0,2160,2334,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <select ");_.b("\n" + i);_.b("        class=\"field select ui-widget ui-corner-all\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,2256,2310,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}});c.pop();}if(_.s(_.f("file",c,p,1),c,p,0,2362,2534,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <select ");_.b("\n" + i);_.b("        class=\"field file ui-widget ui-corner-all\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,2456,2510,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}});c.pop();}if(_.s(_.f("multiselect",c,p,1),c,p,0,2564,2765,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <select ");_.b("\n" + i);_.b("        multiple=true");_.b("\n" + i);_.b("        class=\"field multiselect ui-widget ui-corner-all\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,2687,2741,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}});c.pop();}if(_.s(_.f("docmultiselect",c,p,1),c,p,0,2805,3006,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <select ");_.b("\n" + i);_.b("        multiple=true");_.b("\n" + i);_.b("        class=\"field multiselect ui-widget ui-corner-all\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,2928,2982,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}});c.pop();}if(_.s(_.f("textarea",c,p,1),c,p,0,3043,3221,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <textarea ");_.b("\n" + i);_.b("        class=\"field textarea ui-widget ui-corner-all\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,3143,3197,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}});c.pop();}_.b("    name=\"");_.b(_.v(_.f("name",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("    data-field-subcategory=\"");_.b(_.v(_.f("subcategory",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-charseq=\"");_.b(_.v(_.f("charseq",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-label=\"");_.b(_.v(_.f("label",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-order=\"");_.b(_.v(_.f("order",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-head=\"");_.b(_.v(_.f("head",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-required=\"");_.b(_.v(_.f("required",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-reversal=\"");_.b(_.v(_.f("reversal",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-name=\"");_.b(_.v(_.f("name",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-min=\"");_.b(_.v(_.f("min",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-max=\"");_.b(_.v(_.f("max",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-regex=\"");_.b(_.v(_.f("regex",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-instance=\"");_.b(_.v(_.f("instance",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-group-id=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    id=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);if(_.s(_.f("text",c,p,1),c,p,0,3793,3803,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    />");_.b("\n");});c.pop();}if(_.s(_.f("integer",c,p,1),c,p,0,3827,3837,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    />");_.b("\n");});c.pop();}if(_.s(_.f("rational",c,p,1),c,p,0,3865,3875,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    />");_.b("\n");});c.pop();}if(_.s(_.f("date",c,p,1),c,p,0,3900,3910,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    />");_.b("\n");});c.pop();}if(_.s(_.f("boolean",c,p,1),c,p,0,3934,3965,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    /> Check for true<br />");_.b("\n");});c.pop();}if(_.s(_.f("openboolean",c,p,1),c,p,0,3996,4252,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    >");_.b("\n" + i);_.b("      <option value=\"null\" ");if(_.s(_.f("is_null",c,p,1),c,p,0,4042,4055,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("selected=true");});c.pop();}_.b("></option>");_.b("\n" + i);_.b("      <option value=\"false\" ");if(_.s(_.f("is_false",c,p,1),c,p,0,4119,4132,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("selected=true");});c.pop();}_.b(">False</option>");_.b("\n" + i);_.b("      <option value=\"true\" ");if(_.s(_.f("value",c,p,1),c,p,0,4198,4211,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("selected=true");});c.pop();}_.b(">True</option>");_.b("\n" + i);_.b("    </select>");_.b("\n");});c.pop();}if(_.s(_.f("select",c,p,1),c,p,0,4282,4545,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    >");_.b("\n" + i);if(!_.s(_.f("required",c,p,1),c,p,1,0,0,"")){_.b("      <option value=\"\" ");if(!_.s(_.f("default",c,p,1),c,p,1,0,0,"")){_.b("selected=true");};_.b("></option>");_.b("\n");};if(_.s(_.f("allowed",c,p,1),c,p,0,4412,4516,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <option value=\"");_.b(_.v(_.f("value",c,p,0)));_.b("\" ");if(_.s(_.f("is_default",c,p,1),c,p,0,4462,4475,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("selected=true");});c.pop();}_.b(">");_.b(_.v(_.f("value",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}_.b("    </select>");_.b("\n");});c.pop();}if(_.s(_.f("docselect",c,p,1),c,p,0,4573,4836,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    >");_.b("\n" + i);if(!_.s(_.f("required",c,p,1),c,p,1,0,0,"")){_.b("      <option value=\"\" ");if(!_.s(_.f("default",c,p,1),c,p,1,0,0,"")){_.b("selected=true");};_.b("></option>");_.b("\n");};if(_.s(_.f("allowed",c,p,1),c,p,0,4703,4807,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <option value=\"");_.b(_.v(_.f("value",c,p,0)));_.b("\" ");if(_.s(_.f("is_default",c,p,1),c,p,0,4753,4766,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("selected=true");});c.pop();}_.b(">");_.b(_.v(_.f("value",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}_.b("    </select>");_.b("\n");});c.pop();}if(_.s(_.f("file",c,p,1),c,p,0,4862,5121,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    >");_.b("\n" + i);if(!_.s(_.f("required",c,p,1),c,p,1,0,0,"")){_.b("      <option value=\"\" ");if(!_.s(_.f("default",c,p,1),c,p,1,0,0,"")){_.b("selected=true");};_.b("></option>");_.b("\n");};if(_.s(_.f("allowed",c,p,1),c,p,0,4992,5092,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <option value=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\" ");if(_.s(_.f("is_default",c,p,1),c,p,0,5040,5053,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("selected=true");});c.pop();}_.b(">");_.b(_.v(_.f("key",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}_.b("    </select>");_.b("\n");});c.pop();}if(_.s(_.f("multiselect",c,p,1),c,p,0,5149,5305,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    >");_.b("\n" + i);if(_.s(_.f("allowed",c,p,1),c,p,0,5172,5276,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <option value=\"");_.b(_.v(_.f("value",c,p,0)));_.b("\" ");if(_.s(_.f("is_default",c,p,1),c,p,0,5222,5235,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("selected=true");});c.pop();}_.b(">");_.b(_.v(_.f("value",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}_.b("    </select>");_.b("\n");});c.pop();}if(_.s(_.f("docmultiselect",c,p,1),c,p,0,5343,5499,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    >");_.b("\n" + i);if(_.s(_.f("allowed",c,p,1),c,p,0,5366,5470,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <option value=\"");_.b(_.v(_.f("value",c,p,0)));_.b("\" ");if(_.s(_.f("is_default",c,p,1),c,p,0,5416,5429,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("selected=true");});c.pop();}_.b(">");_.b(_.v(_.f("value",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}_.b("    </select>");_.b("\n");});c.pop();}if(_.s(_.f("textarea",c,p,1),c,p,0,5534,5683,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    >");if(_.s(_.f("default",c,p,1),c,p,0,5552,5565,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.v(_.f("default",c,p,0)));});c.pop();}_.b("</textarea>");_.b("\n" + i);_.b("    <span title=\"Expand/Shrink Text Box\" class=\"expander\" data-group-id=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\"></span>");_.b("\n");});c.pop();}_.b("  </div>");_.b("\n");});c.pop();}return _.fl();;}),
  'fieldset' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<div ");_.b("\n" + i);_.b("  data-group-id=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("  class=\"fields ui-widget ui-widget-content ui-corner-all padded\">");_.b("\n" + i);if(_.s(_.f("multiple",c,p,1),c,p,0,116,181,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <a href=\"#\" class=\"remove-button link-button\">Remove</a >");_.b("\n");});c.pop();}_.b("</div>");_.b("\n");return _.fl();;}),
  'index-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr class=\"change-header\" id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,64,179,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    <ul class=\"head-elements\">");_.b("\n" + i);if(_.s(_.f("display_key",c,p,1),c,p,0,247,362,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <li>");_.b("\n" + i);_.b("          <a href=\"#");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("            class=\"view-document-link\">");_.b(_.v(_.d(".",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("        </li>");_.b("\n");});c.pop();}_.b("    </ul>");_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    <ul class=\"reversal-elements\">");_.b("\n" + i);if(_.s(_.f("value",c,p,1),c,p,0,455,487,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <li>");_.b(_.v(_.d(".",c,p,0)));_.b("</li>");_.b("\n");});c.pop();}_.b("    </ul>");_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'index-listing' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<table>");_.b("\n" + i);_.b("  <thead>");_.b("\n" + i);_.b("    <th>Name</th>");_.b("\n" + i);_.b("    <th>Doctype</th>");_.b("\n" + i);_.b("  </thead>");_.b("\n" + i);_.b("  <tbody>");_.b("\n" + i);if(_.s(_.f("rows",c,p,1),c,p,0,91,210,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <tr>");_.b("\n" + i);_.b("      <th><a href=\"#\" data-index-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b(_.v(_.d("key.1",c,p,0)));_.b("</a></th> ");_.b("\n" + i);_.b("      <td>");_.b(_.v(_.d("key.0",c,p,0)));_.b("</td>");_.b("\n" + i);_.b("    </tr>");_.b("\n");});c.pop();}_.b("  </tbody>");_.b("\n" + i);_.b("</table>");return _.fl();;}),
  'index-options' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<option></option>");_.b("\n" + i);if(_.s(_.f("rows",c,p,1),c,p,0,27,74,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<option value=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b(_.v(_.d("key.1",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}return _.fl();;}),
  'paged-listing' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<nav class=\"pager\">");_.b("\n" + i);_.b("<a");_.b("\n" + i);_.b("  href=\"#\" ");_.b("\n" + i);_.b("  title=\"Previous Page\"");_.b("\n" + i);_.b("  id=\"previous-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-page\"");_.b("\n" + i);_.b("  class=\"pager-button link-button\"");_.b("\n" + i);_.b(">Prev</a> ");_.b("\n" + i);_.b("<a");_.b("\n" + i);_.b("  href=\"#\"");_.b("\n" + i);_.b("  title=\"Next Page\"");_.b("\n" + i);_.b("  class=\"pager-button link-button\"");_.b("\n" + i);_.b("  id=\"next-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-page\"");_.b("\n" + i);if(_.s(_.f("lastpage",c,p,1),c,p,0,322,351,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    data-last-page=\"true\"");_.b("\n");});c.pop();}if(_.s(_.f("lastrow",c,p,1),c,p,0,379,448,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    data-startkey=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-startid=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n");});c.pop();}_.b(">Next</a>");_.b("\n" + i);_.b("</nav>");_.b("\n" + i);_.b("<div class=\"total-rows-info\">");_.b("\n" + i);_.b("  <b>Total</b>: ");_.b(_.v(_.f("total_rows",c,p,0)));_.b("\n" + i);_.b("</div>");_.b("\n" + i);_.b("<table>");_.b("\n" + i);if(_.s(_.f("rows",c,p,1),c,p,0,567,595,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("listed-element",c,p,"    "));});c.pop();}_.b("</table>");_.b("\n");return _.fl();;}),
  'preview-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr class=\"change-header\" id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,64,179,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    <ul class=\"head-elements\">");_.b("\n" + i);if(_.s(_.f("display_key",c,p,1),c,p,0,247,279,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <li>");_.b(_.v(_.d(".",c,p,0)));_.b("</li>");_.b("\n");});c.pop();}_.b("    </ul>");_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    <ul class=\"reversal-elements\">");_.b("\n" + i);_.b("      ");_.b(_.v(_.f("value",c,p,0)));_.b("\n" + i);_.b("    </ul>");_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'project-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr>");_.b("\n" + i);_.b("  <td><a href=\"/projects/");_.b(_.v(_.f("id",c,p,0)));_.b("/doctypes/main\">");if(_.s(_.f("doc",c,p,1),c,p,0,62,72,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.v(_.f("name",c,p,0)));});c.pop();}_.b("</a></td>");_.b("\n" + i);_.b("  <td>");if(_.s(_.f("doc",c,p,1),c,p,0,104,121,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.v(_.f("description",c,p,0)));});c.pop();}_.b("</td>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    <a href=\"/projects/");_.b(_.v(_.f("id",c,p,0)));_.b("/config\" ");_.b("\n" + i);_.b("       class=\"project-configure-button link-button\">Configure</a>");_.b("\n" + i);_.b("    <a href=\"#\" ");_.b("\n" + i);_.b("       class=\"project-delete-button link-button\" ");_.b("\n" + i);_.b("       id=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\">Delete</button>");_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'project-listing' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");if(_.s(_.f("rows",c,p,1),c,p,0,9,34,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("project-element",c,p,"  "));});c.pop();}return _.fl();;}),
  'search-field-item' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<a class='search-field-item' ");_.b("\n" + i);_.b("  title='click to remove' ");_.b("\n" + i);_.b("  data-field-field='");_.b(_.v(_.f("field",c,p,0)));_.b("' ");_.b("\n" + i);_.b("  href='#'>");_.b(_.v(_.f("fieldLabel",c,p,0)));_.b("</a>");_.b("\n");return _.fl();;}),
  'set-listing' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<div class=\"total-rows-info\">");_.b("\n" + i);_.b("  <b>Total</b>: <span id=\"total-set-rows\">");_.b(_.v(_.f("total",c,p,0)));_.b("</span>");_.b("\n" + i);_.b("</div>");_.b("\n" + i);_.b("<div id=\"save-set-results\">");_.b("\n" + i);_.b("  <a href=\"#\">(Save Selected)</a>");_.b("\n" + i);_.b("</div>");_.b("\n" + i);_.b("<table id=\"set-elements\">");_.b("\n" + i);_.b("  <thead>");_.b("\n" + i);_.b("    <tr>");_.b("\n" + i);_.b("      <td>");_.b("\n" + i);_.b("        <input type=\"checkbox\" id=\"select-all-set-elements\" title=\"Click to select or deselect all elements\" />");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);_.b("      <th>");_.b("\n" + i);_.b("        Elements");_.b("\n" + i);_.b("      </th>");_.b("\n" + i);_.b("    </tr>");_.b("\n" + i);_.b("  </thead>");_.b("\n" + i);_.b("  <tbody>");_.b("\n" + i);if(_.s(_.f("elements",c,p,1),c,p,0,435,674,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <tr>");_.b("\n" + i);_.b("      <td>");_.b("\n" + i);_.b("        <input type=\"checkbox\" class=\"set-element-selection\" title=\"Click to select element\" />");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);_.b("      <td>");_.b("\n" + i);_.b("        <a class=\"view-document-link\" href=\"#");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b(_.v(_.f("context",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);_.b("    </tr>");_.b("\n");});c.pop();}_.b("  </tbody>");_.b("\n" + i);_.b("</table>");_.b("\n");return _.fl();;}),
  'set-options' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<option></option>");_.b("\n" + i);if(_.s(_.f("names",c,p,1),c,p,0,28,66,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<option value=\"");_.b(_.v(_.d(".",c,p,0)));_.b("\">");_.b(_.v(_.d(".",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}return _.fl();;}),
  'simple-to-form-array' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<ol>");_.b("\n" + i);if(_.s(_.f("value",c,p,1),c,p,0,17,51,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("simple-to-form-field",c,p,"    "));});c.pop();}_.b("</ol>");_.b("\n");return _.fl();;}),
  'simple-to-form-field' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<li>");_.b("\n" + i);if(_.s(_.f("key",c,p,1),c,p,0,15,63,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <label for=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\">");_.b(_.v(_.f("key",c,p,0)));_.b("</label>");_.b("\n");});c.pop();}if(_.s(_.f("text",c,p,1),c,p,0,83,156,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <textarea ");if(_.s(_.f("key",c,p,1),c,p,0,106,122,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("name=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\"");});c.pop();}_.b(">");_.b(_.v(_.f("value",c,p,0)));_.b("</textarea>");_.b("\n");});c.pop();}if(_.s(_.f("string",c,p,1),c,p,0,179,260,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <input type=\"text\" ");if(_.s(_.f("key",c,p,1),c,p,0,211,227,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("name=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\"");});c.pop();}_.b(" value=\"");_.b(_.v(_.f("value",c,p,0)));_.b("\"/>");_.b("\n");});c.pop();}if(_.s(_.f("bool",c,p,1),c,p,0,283,364,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <input type=\"text\" ");if(_.s(_.f("key",c,p,1),c,p,0,315,331,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("name=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\"");});c.pop();}_.b(" value=\"");_.b(_.v(_.f("value",c,p,0)));_.b("\"/>");_.b("\n");});c.pop();}if(_.s(_.f("number",c,p,1),c,p,0,387,470,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <input type=\"number\" ");if(_.s(_.f("key",c,p,1),c,p,0,421,437,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("name=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\"");});c.pop();}_.b(" value=\"");_.b(_.v(_.f("value",c,p,0)));_.b("\"/>");_.b("\n");});c.pop();}if(_.s(_.f("array",c,p,1),c,p,0,494,691,"{{ }}")){_.rs(c,p,function(c,p,_){if(_.s(_.f("key",c,p,1),c,p,0,507,617,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <fieldset>");_.b("\n" + i);_.b("        <legend>");_.b(_.v(_.f("key",c,p,0)));_.b("</legend>");_.b("\n" + i);_.b(_.rp("simple-to-form-array",c,p,"        "));_.b("      </fieldset>");_.b("\n");});c.pop();}if(_.s(_.f("index",c,p,1),c,p,0,640,678,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("simple-to-form-array",c,p,"      "));});c.pop();}});c.pop();}if(_.s(_.f("object",c,p,1),c,p,0,715,914,"{{ }}")){_.rs(c,p,function(c,p,_){if(_.s(_.f("key",c,p,1),c,p,0,728,839,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <fieldset>");_.b("\n" + i);_.b("        <legend>");_.b(_.v(_.f("key",c,p,0)));_.b("</legend>");_.b("\n" + i);_.b(_.rp("simple-to-form-object",c,p,"        "));_.b("      </fieldset>");_.b("\n");});c.pop();}if(_.s(_.f("index",c,p,1),c,p,0,862,901,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("simple-to-form-object",c,p,"      "));});c.pop();}});c.pop();}_.b("</li>");_.b("\n");return _.fl();;}),
  'simple-to-form-object' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<ul>");_.b("\n" + i);if(_.s(_.f("value",c,p,1),c,p,0,17,51,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("simple-to-form-field",c,p,"    "));});c.pop();}_.b("</ul>");_.b("\n");return _.fl();;}),
  'simple-to-form' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<form>");_.b("\n" + i);if(_.s(_.f("obj",c,p,1),c,p,0,17,110,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <ul>");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,44,86,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("simple-to-form-field",c,p,"        "));});c.pop();}_.b("    </ul>");_.b("\n");});c.pop();}_.b("</form>");_.b("\n");return _.fl();;}),
  'worksheet' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<table id=\"worksheet-table\">");_.b("\n" + i);_.b("  <thead>");_.b("\n" + i);_.b("    <tr class=\"header-row\">");_.b("\n" + i);_.b("      <td id=\"select-all-worksheet-rows-cell\"");_.b("\n" + i);_.b("        class=\"select-column\">");_.b("\n" + i);_.b("        <input ");_.b("\n" + i);_.b("          id=\"select-all-worksheet-rows\"");_.b("\n" + i);_.b("          type=\"checkbox\"");_.b("\n" + i);_.b("          title=\"Click to select all rows\">");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,303,1494,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <th ");_.b("\n" + i);_.b("          class=\"worksheet-handle-header fieldset handle-column ");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          title=\"");_.b(_.v(_.f("label",c,p,0)));_.b("\">");_.b("\n" + i);_.b("          <div>");_.b("\n" + i);_.b("            <span>");_.b("\n" + i);_.b("              <a class=\"fieldset-handle\" ");_.b("\n" + i);_.b("                data-field-fieldset=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("                href=\"#\">");_.b(_.v(_.f("label",c,p,0)));_.b("</a></span></div>");_.b("\n" + i);_.b("        </th>");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,634,1476,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          <th");_.b("\n" + i);_.b("            class=\"");if(_.s(_.f("multiple",c,p,1),c,p,0,681,689,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("multiple");});c.pop();}_.b(" ");if(_.s(_.f("collapse",c,p,1),c,p,0,716,724,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("collapse");});c.pop();}_.b(" ");_.b(_.v(_.f("fieldset",c,p,0)));_.b(" ");_.b(_.v(_.f("_id",c,p,0)));_.b(" worksheet-handle-header field handle-column\"");_.b("\n" + i);_.b("            title=\"");_.b(_.v(_.f("label",c,p,0)));_.b("\">");_.b("\n" + i);_.b("            <div>");_.b("\n" + i);_.b("              <span>");_.b("\n" + i);_.b("                <a class=\"field-handle\" ");_.b("\n" + i);_.b("                  data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("                  href=\"#\">");_.b(_.v(_.f("label",c,p,0)));_.b("</a></span></div>");_.b("\n" + i);_.b("          </th>");_.b("\n" + i);_.b("          <th");_.b("\n" + i);_.b("            class=\"");_.b(_.v(_.f("fieldset",c,p,0)));_.b(" ");_.b(_.v(_.f("_id",c,p,0)));_.b(" field-column\"");_.b("\n" + i);_.b("            title=\"");_.b(_.v(_.f("label",c,p,0)));_.b("\">");_.b("\n" + i);_.b("            <a class=\"field-header\" ");_.b("\n" + i);_.b("              data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("              href=\"#\">");_.b(_.v(_.f("label",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("            <input ");_.b("\n" + i);_.b("              class=\"select-worksheet-column\"");_.b("\n" + i);_.b("              data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("              type=\"checkbox\"");_.b("\n" + i);_.b("              title=\"Click to select column\">");_.b("\n" + i);_.b("          </td>");_.b("\n");});c.pop();}});c.pop();}_.b("    </tr>");_.b("\n" + i);_.b("  </thead>");_.b("\n" + i);_.b("  <tbody>");_.b("\n" + i);_.b("    <%#rows%>");_.b("\n" + i);_.b("      <tr id=\"worksheet-row-<% _id %>\"");_.b("\n" + i);_.b("        class=\"body-row\">");_.b("\n" + i);_.b("        <td class=\"select-column\">");_.b("\n" + i);_.b("          <input ");_.b("\n" + i);_.b("            class=\"select-worksheet-row\"");_.b("\n" + i);_.b("            data-row=\"worksheet-row-<% _id %>\"");_.b("\n" + i);_.b("            type=\"checkbox\"");_.b("\n" + i);_.b("            title=\"Click to select row\">");_.b("\n" + i);_.b("        </td>");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,1865,2890,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          <td class=\"");_.b(_.v(_.f("_id",c,p,0)));_.b(" fieldset handle-column\"></td>");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,1948,2870,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("            <td class=\"");if(_.s(_.f("multiple",c,p,1),c,p,0,1985,1993,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("multiple");});c.pop();}_.b(" ");if(_.s(_.f("collapse",c,p,1),c,p,0,2020,2028,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("collapse");});c.pop();}_.b(" ");_.b(_.v(_.f("_id",c,p,0)));_.b(" ");_.b(_.v(_.f("fieldset",c,p,0)));_.b(" field handle-column\"></td>");_.b("\n" + i);_.b("            <td");_.b("\n" + i);_.b("              class=\"");if(_.s(_.f("multiple",c,p,1),c,p,0,2144,2152,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("multiple");});c.pop();}_.b(" ");if(_.s(_.f("collapse",c,p,1),c,p,0,2179,2187,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("collapse");});c.pop();}_.b(" ");_.b(_.v(_.f("fieldset",c,p,0)));_.b(" ");_.b(_.v(_.f("_id",c,p,0)));_.b(" field-column\"");_.b("\n" + i);_.b("              data-field-fieldset=\"");_.b(_.v(_.f("fieldset",c,p,0)));_.b("\"");_.b("\n" + i);_.b("              data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("              <%#");_.b(_.v(_.f("_id",c,p,0)));_.b("%>");_.b("\n" + i);_.b("                <%#multiple%>");_.b("\n" + i);_.b("                <ol>");_.b("\n" + i);_.b("                  <%#items%>");_.b("\n" + i);_.b("                    <li");_.b("\n" + i);_.b("                      data-field-fieldset_instance=\"<% fieldset_instance %>\"");_.b("\n" + i);_.b("                      data-field-field_instance=\"<% field_instance %>\"><% value %></li>");_.b("\n" + i);_.b("                  <%/items%>");_.b("\n" + i);_.b("                </ol>");_.b("\n" + i);_.b("                <%/multiple%>");_.b("\n" + i);_.b("                <%#single%>");_.b("\n" + i);_.b("                  <span><% value %></span>");_.b("\n" + i);_.b("                <%/single%>");_.b("\n" + i);_.b("              <%/");_.b(_.v(_.f("_id",c,p,0)));_.b("%>");_.b("\n" + i);_.b("            </td>");_.b("\n");});c.pop();}});c.pop();}_.b("      </tr>");_.b("\n" + i);_.b("    <%/rows%>");_.b("\n" + i);_.b("  </tbody>");_.b("\n" + i);_.b("</table>");_.b("\n");return _.fl();;})
},
r = function(n) {
  var tn = t[n];
  return function(c, p, i) {
    return tn.render(c, p || t, i);
  }
};
module.exports = {
  templates : t,
  'changelog-element' : r('changelog-element'),
  'charseqs-element' : r('charseqs-element'),
  'config-maintenance' : r('config-maintenance'),
  'doctypes-element' : r('doctypes-element'),
  'document-edit' : r('document-edit'),
  'document-search-results' : r('document-search-results'),
  'document-search' : r('document-search'),
  'document-view-field' : r('document-view-field'),
  'document-view-tree' : r('document-view-tree'),
  'document-view' : r('document-view'),
  'fields' : r('fields'),
  'fieldset' : r('fieldset'),
  'index-element' : r('index-element'),
  'index-listing' : r('index-listing'),
  'index-options' : r('index-options'),
  'paged-listing' : r('paged-listing'),
  'preview-element' : r('preview-element'),
  'project-element' : r('project-element'),
  'project-listing' : r('project-listing'),
  'search-field-item' : r('search-field-item'),
  'set-listing' : r('set-listing'),
  'set-options' : r('set-options'),
  'simple-to-form-array' : r('simple-to-form-array'),
  'simple-to-form-field' : r('simple-to-form-field'),
  'simple-to-form-object' : r('simple-to-form-object'),
  'simple-to-form' : r('simple-to-form'),
  'worksheet' : r('worksheet')
};
},{"hogan.js":24}],53:[function(require,module,exports){
// # Handle Ajax Requests
//
// *Implicit depends:* DOM
//
// Ajax helpers and request behavior standardization -- such as
// displaying a spinner.

// ## Variable Definitions

var flash = require('flash');

// ## Internal Functions

// The spinner element.
var spinner = function () {
  'use strict';

  return document.getElementById('loading');
};

// Called when request is sent.
var ajaxStart = function () {
  'use strict';

  spinner().style.display = 'block';

  return 'ajax-started';
};

// Stop the spinner when request is complete.
var ajaxStop = function () {
  'use strict';

  spinner().style.display = 'none';

  return 'ajax-stopped';
};

var makeMessage = function (response) {
  'use strict';

  var retval = response.fieldname ? response.fieldname + ' ' : '';

  return retval + response.message;
};

// Run on request completion with callback and default behavior in
// case of common errors.
var complete = function (req, success, statusCallbacks) {
  'use strict';

  if (statusCallbacks && statusCallbacks[req.status]) {
    statusCallbacks[req.status](req);
  } else if (req.status >= 200 && req.status < 300 && success) {
    success(req);
  } else if (req.status === 500) {
    flash.error('Unknown Server Error', 'Please report that you received this message');
  } else if (req.status >= 400) {
    var msg;

    if (req.response && typeof req.response === 'string') {
      msg = makeMessage(JSON.stringify(req.response));
      // TODO: determine if the following condition is needed.
    } else if (req.response && req.response instanceof Object) {
      msg = makeMessage(req.response);
    } else if (req.status === 404) {
      msg = 'The document was not found on the server.';
    } else {
      msg = 'That is all.';
    }

    flash.error(req.statusText, msg);
  }

  return 'ajax-complete';
};

// Returns an `onreadystatechange` handler.
var stateChange = function (req, success, statusCallbacks) {
  'use strict';

  return function () {
    switch (req.readyState) {
    case 1:
      return ajaxStart();
    case 4:
      ajaxStop();
      return complete(req, success, statusCallbacks);
    default:
      return 'waiting';
    }
  };
};

// Convert object to JSON if needed.
var processObject = function (obj) {
  'use strict';

  if (obj instanceof Object) {
    return JSON.stringify(obj);
  } else if (typeof obj === 'string') {
    return obj;
  } else {
    return '';
  }
};

// ## Exported Functions

// Perform an Ajax action with a URL, object to be translated to JSON,
// an HTTP method and a function to be run on completion.
var send = function (url, obj, method, success, statusCallbacks) {
  'use strict';

  var dataObj = processObject(obj);
  var req = new XMLHttpRequest();

  req.onreadystatechange = stateChange(req, success, statusCallbacks);
  req.open(method, url);
  req.responseType = 'json';
  req.setRequestHeader('Content-Type', 'application/json');
  req.setRequestHeader('Accept', 'application/json');

  req.send(dataObj);

  return true;
};

// Simplified `send` for GET requests.
var get = function (url, success, statusCallbacks) {
  'use strict';

  return send(url, false, 'GET', success, statusCallbacks);
};

// Simplified `send` for DELETE requests.
var del = function (url, success, statusCallbacks) {
  'use strict';

  return send(url, false, 'DELETE', success, statusCallbacks);
};

// Simplified `send` for POST requests.
var post = function (url, obj, success, statusCallbacks) {
  'use strict';

  return send(url, obj, 'POST', success, statusCallbacks);
};

// Simplified `send` for PUT requests.
var put = function (url, obj, success, statusCallbacks) {
  'use strict';

  return send(url, obj, 'PUT', success, statusCallbacks);
};

// Perform an Ajax GET action, expecting HTML, which is the old way.
var legacyHTMLGet = function (url, callback) {
  'use strict';

  var req = new XMLHttpRequest();

  req.open('GET', url);

  req.onreadystatechange = stateChange(req, callback);

  req.send();

  return true;
};

exports.send = send;
exports.post = post;
exports.put = put;
exports.del = del;
exports.get = get;
exports.legacyHTMLGet = legacyHTMLGet;

},{"flash":128}],54:[function(require,module,exports){
// # The Client Code Entry Point
//
// *Implicit depends:* DOM
//
// This is the entry point for the client side code. This is where
// basic initializations take place and helper functions are added to
// JavaScript Objects.

// ## Variable Definitions

var exports = module.exports;
var clickDispatch = require('click-dispatch').clickDispatch;
var dblclickDispatch = require('dblclick-dispatch').dblclickDispatch;
var changes = require('changes').changes;
var keystrokes = require('keystrokes').keystrokes;
var form = require('form');

// These are the basic sub-application entry points.
var documents = require('documents/documents');
var fm = require('file_manager/fm');
var ilistingui = require('index_tool/ilistingui');
var projectui = require('projects/projectui');
var config = require('config/config');

// ## Extensions to String and Array Objects

// ### Functions added to String

// This is a poorly implement `isBlank` predicate.
String.prototype.isBlank = function () {
  'use strict';

  return ((/^\s*$/).test(this) && !(/\S/).test(this));
};

// Remove white space at the beginning and end of string.
String.prototype.trim = function () {
  'use strict';

  return this.replace(/^\s+/, '').replace(/\s+$/, '');
};

// Camel case string
String.prototype.cc = function () {
  'use strict';

  return this.replace(/-./, function (substr) {
    return substr.toUpperCase()[1];
  });
};

// ### Functions added to Array

// Remove white space on all strings in array.
Array.prototype.trimAll = function () {
  'use strict';

  return this.map(function (i) {
    return i.trim();
  }).filter(function (i) {
    return !i.match(/^$/);
  });
};

// ### Functions added to Element

if (Element.prototype.mozMatchesSelector) {
  Element.prototype.matches = Element.prototype.mozMatchesSelector;
} else if (Element.prototype.webkitMatchesSelector) {
  Element.prototype.matches = Element.prototype.webkitMatchesSelector;
} else {
  throw 'This browser is not supported at this time. An implementation of Element.matches is needed https://developer.mozilla.org/en-US/docs/Web/API/Element.matches';
}

// ## Initialization

// Using the function for running code after the page loads.
var init = function () {
  'use strict';

  // All clicks handled centraly
  document.body.onclick = clickDispatch;

  // All double clicks handled centraly
  document.body.ondblclick = dblclickDispatch;

  // Other event handling
  keystrokes();
  changes();

  // Initialize any data fields, which use JQueryUI.
  form.initDateFields();

  // ### Determine the sub-application.

  // Detect if this is the configuration sub-application
  if (document.getElementById('all-config-container')) {
    config.init();
  }

  // Detect if this is the document editing sub-application
  if (document.getElementById('all-document-container')) {
    documents.init();
  }

  // Detect if this is the file manager sub-application
  if (document.getElementById('file-upload')) {
    fm.init();
  }

  // Detect if this is the index tool sub-application
  if (document.getElementById('all-index-container')) {
    ilistingui.init();
  }

  // Detect if this is the project creation sub-application
  if (document.getElementById('projects-container')) {
    projectui.init();
  }
};

document.onreadystatechange = function () {
  'use strict';

  if (document.readyState === 'complete') {
    init();
  }
};

},{"changes":106,"click-dispatch":107,"config/config":109,"dblclick-dispatch":113,"documents/documents":117,"file_manager/fm":127,"form":129,"index_tool/ilistingui":137,"keystrokes":142,"projects/projectui":145}],55:[function(require,module,exports){
// # Change Event Handling
//
// *Implicit depends:* DOM
//
// Like [`click-dispatch.js`](./click-dispatch.html) I would like
// to centralize the 'change' events. This is a start and a bit of
// an experiment to isolate and route input.

// ## Variable Definitions

var searchui = require('documents/searchui');
var newDialog = require('index_tool/new-dialog');

// ## Exported Functions

// Run to add event listeners to `document`.
var changes = function () {
  'use strict';

  var changeTargets = [];

  // ### Search UI Change Events

  changeTargets['document-search-exclude'] = function (e) {
    searchui.toggleExclusion();

    return e;
  };

  changeTargets['document-search-invert'] = function (e) {
    searchui.toggleInversion();

    return e;
  };

  // ### New index

  changeTargets['index-doctype-input'] = function (e) {
    newDialog.doctypeInputChange();

    return e;
  };

  changeTargets['index-fieldset-input'] = function (e) {
    newDialog.fieldsetInputChange();

    return e;
  };

  document.onchange = function (e) {
    if (e.target && changeTargets[e.target.id]) {
      changeTargets[e.target.id](e);
    }

    return e;
  };

  return document;
};

exports.changes = changes;

},{"documents/searchui":122,"index_tool/new-dialog":139}],56:[function(require,module,exports){
// # Dispatching click events
//
// *Implicit depends:* DOM, JQuery, JQueryUI
//
// Almost all click events that are handled by the system are listed
// here. With [sender.js](./sender.html) and other dispatchers,
// the hope is to centralize effects and interdependencies. At some
// point I may use a more sophisticated approach.

// ## Variable Definitions

var S = require('sender');
var dispatcher = require('dispatcher').dispatcher;
var panelToggler = require('panel-toggle').panelToggler;
var editui = require('documents/editui');
var viewui = require('documents/viewui');
var indexui = require('documents/indexui');
var setsui = require('documents/setsui');
var searchui = require('documents/searchui');
var worksheetui = require('documents/worksheetui');
var fieldsets = require('documents/fieldsets');
var ieditui = require('index_tool/ieditui');
var form = require('form');
var projectui = require('projects/projectui');
var fm = require('file_manager/fm');
var maintenanceui = require('config/maintenanceui');

// ## Internal Functions

// The calling of the default action will seem redundant for a while
// until a more full refactor can be done and all actions are default.
var defaultAction = function (t) {
  'use strict';

  return S.sender(t.id.slice(0, -7));
};

// ## Exported Functions

// Given a click event, determine what action to take based on the
// click target.
var clickDispatch = function (e) {
  'use strict';

  var action = dispatcher({
    // ### Config

    '.edit-doctype-link': function (t) {
      return S.sender('edit-doctype-requested', 'doctypes/' + t.getAttribute('href').slice(1));
    },
    '#doctypes-add-button': function (t) {
      return defaultAction(t);
    },
    '#maintenance-upgrade-button': function (t) {
      return maintenanceui.upgradeButton(t);
    },
    '#config-save-button': function (t) {
      return defaultAction(t);
    },
    '#config-delete-button': function (t) {
      return defaultAction(t);
    },
    '#config-create-button': function (t) {
      return defaultAction(t);
    },
    '#config-move-up-button': function (t) {
      return defaultAction(t);
    },
    '#config-move-down-button': function (t) {
      return defaultAction(t);
    },
    '#config-remove-element-button': function (t) {
      return defaultAction(t);
    },
    '#config-add-object-button': function (t) {
      return defaultAction(t);
    },
    '#config-add-array-button': function (t) {
      return defaultAction(t);
    },
    '#config-add-text-button': function (t) {
      return defaultAction(t);
    },
    '#config-add-child-text-button': function (t) {
      return defaultAction(t);
    },
    '#config-add-child-object-button': function (t) {
      return defaultAction(t);
    },
    '#config-add-child-array-button': function (t) {
      return defaultAction(t);
    },
    '#config-clear-form-button': function (t) {
      return defaultAction(t);
    },
    '#config-copy-button': function (t) {
      return defaultAction(t);
    },
    '#config-cut-button': function (t) {
      return defaultAction(t);
    },
    '#config-paste-button': function (t) {
      return defaultAction(t);
    },
    '#config-paste-child-button': function (t) {
      return defaultAction(t);
    },
    '#config-promote-button': function (t) {
      return defaultAction(t);
    },
    '#config-demote-button': function (t) {
      return defaultAction(t);
    },
    '#edit-form ol > li': function (t) {
      return S.sender('config-mark-line', t);
    },

    // ### Documents

    '.add-button': function (t) {
      fieldsets.initFieldset(t, false, true);
    },
    '.remove-button': function (t) {
      fieldsets.removeFieldset(t);
    },
    '#save-document-button': function (t) {
      editui.save();
    },
    '#create-document-button': function (t) {
      editui.create();
    },
    '#clear-document-button': function (t) {
      editui.clear();
    },
    '.expander': function (t) {
      editui.toggleTextarea(t);
    },
    'label span.ui-icon-help': function (t) {
      editui.showHelpDialog(t);
    },
    '#document-edit-button': function (t) {
      viewui.edit(t);
    },
    '#document-delete-button': function (t) {
      viewui.confirmDelete();
    },
    '#document-restore-button': function (t) {
      viewui.confirmRestore();
    },
    '#document-view-tree > ul > li > b': function (t) {
      viewui.collapseToggle(t);
    },
    '.revision-link': function (t) {
      viewui.fetchRevision(t);
    },
    '#search-all-fields-switch a': function () {
      searchui.allFields();
    },
    '.search-field-item': function (t) {
      searchui.removeField(t);
    },
    '.select-results': function (t) {
      searchui.toggleSelection(t);
    },
    '#save-search-results a': function () {
      $('#new-set-target-input').val('search');
      $('#new-set-dialog').show();
    },
    '#save-set-results a': function () {
      $('#new-set-target-input').val('sets');
      $('#new-set-dialog').show();
    },
    '#new-set-save-button': function () {
      S.sender('new-set-form-submit');
    },
    '#select-all-set-elements': function (t) {
      setsui.toggleSelectAll(t);
    },
    '.view-document-link span': function (t) {
      var parent = t[0].parentNode;
      indexui.load(parent);
    },
    '.view-document-link': function (t) {
      indexui.load(t);
    },
    '.select-worksheet-column': function (t) {
      var target = $(t);
      var checked = target.is(':checked');
      var field = target.attr('data-field-field');
      worksheetui.columnSelection(field, checked);
    },
    '.select-worksheet-row': function (t) {
      var target = $(t);
      var checked = target.is(':checked');
      var row = target.attr('data-row');
      worksheetui.rowSelection(row, checked);
    },
    '#select-all-worksheet-rows': function (t) {
      var checked = $(t).is(':checked');
      worksheetui.selectAllRows(checked);
    },
    '#toggle-handles': function (t) {
      worksheetui.showHandles();
    },
    '.fieldset-handle': function (t) {
      worksheetui.showFieldset($(t).attr('data-field-fieldset'));
    },
    '.field-handle': function (t) {
      worksheetui.showField($(t).attr('data-field-field'));
    },
    '.field-header': function (t) {
      worksheetui.hideField($(t).attr('data-field-field'));
    },

    // ### Index Tool

    '#new-index-button': function (t) {
      ieditui.newCond();
    },
    '.remove-condition-button': function (t) {
      ieditui.remCond(t);
    },
    '#delete-index-button': function (t) {
      ieditui.del();
    },
    '#save-index-button': function (t) {
      ieditui.save();
    },
    '#replace-button': function (t) {
      ieditui.replace();
    },
    '#add-index-condition-button': function (t) {
      ieditui.addCond();
    },
    '#index-index-listing a': function (t) {
      ieditui.init(t);
    },

    // ### Project

    '#create-project': function () {
      projectui.add().dialog('open');
    },
    '.project-delete-button': function (t) {
      projectui.del(t);
    },

    // ### File Manager

    '#up-dir': function () {
      fm.upDir();
    },
    '#root-dir': function () {
      fm.rootDir();
    },
    '.dir': function (t) {
      fm.goDir(t);
    },
    '.delete-file-button': function (t) {
      fm.deleteFile(t);
    },
    '.edit-file-button': function (t) {
      fm.editFile(t);
    },

    // ### General

    '.toggler': function (t) {
      form.toggle(t);
    },
    '.cancel-dialog': function (t) {
      form.cancelDialog(t);
    },
    '#panel-toggle li': function (t) {
      panelToggler(t);
    }
  });

  action(e);
};

exports.clickDispatch = clickDispatch;

},{"config/maintenanceui":112,"dispatcher":114,"documents/editui":118,"documents/fieldsets":119,"documents/indexui":120,"documents/searchui":122,"documents/setsui":123,"documents/viewui":125,"documents/worksheetui":126,"file_manager/fm":127,"form":129,"index_tool/ieditui":134,"panel-toggle":144,"projects/projectui":145,"sender":146}],57:[function(require,module,exports){
// # Charseq Listing
//
// *Implicit depends:* DOM
//
// Loads a listing of charseqs that can be edited.

var templates = require('templates');
var pager = require('pager').pager;

// ## Exported Functions

// Return the 'prefix' which is used in id and class names for
// elements used to page through these values.
var prefix = function () {
  'use strict';

  return 'charseqs';
};

// Called by a keystroke event handler when user changes form values.
var get = function () {
  'use strict';

  var url = 'config/' + prefix();
  var target = document.getElementById(prefix() + '-listing');

  pager({
    prefix: prefix(),
    url: url,
    target: target
  }).get();

  return 'charseq-listing-requested';
};

var init = function () {
  'use strict';

  get();

  return 'charsequi-initialized';
};

exports.init = init;
exports.get = get;
exports.prefix = prefix;

},{"pager":143,"templates":52}],58:[function(require,module,exports){
// # Config Sub-App Init
//
// *Implicit depends:* DOM, JQuery
//
// Initialization of the sub-application used to configure the system and
// define doctypes. It also includes code for the upgrade button element,
// for very little reason.

// ## Variable Definitions

var doctypeui = require('config/doctypeui');
var maintenanceui = require('config/maintenanceui');
var charsequi = require('config/charsequi');
var editui = require('config/editui');

// ## Internal Functions

// ## Exported Functions

// Run initialization code for the configuration sub-application.
var init = function () {
  'use strict';

  editui.init();
  doctypeui.init();
  charsequi.init();
  maintenanceui.init();

  return 'config-initialized';
};

exports.init = init;

},{"config/charsequi":108,"config/doctypeui":110,"config/editui":111,"config/maintenanceui":112}],59:[function(require,module,exports){
// # Doctype Listing
//
// *Implicit depends:* DOM
//
// Loads a listing of doctypes that can be edited.

var templates = require('templates');
var pager = require('pager').pager;
var S = require('../sender.js');
var uuid = require('node-uuid');

// ## Exported Functions

// Return the 'prefix' which is used in id and class names for
// elements used to page through these values.
var prefix = function () {
  'use strict';

  return 'doctypes';
};

// Called by a keystroke event handler when user changes form values.
var get = function () {
  'use strict';

  var url = 'config/' + prefix();
  var target = document.getElementById(prefix() + '-listing');

  pager({
    prefix: prefix(),
    url: url,
    target: target
  }).get();

  return 'doctype-listing-requested';
};

// Initialization
var init = function () {
  'use strict';

  get();

  return 'doctypeui-initialized';
};

// Begin the process of adding a doctype by sending JSON to the
// editor.
var addDoctype = function () {
  'use strict';

  var obj = {
    _id: uuid.v4().replace(/-/g, ''),
    category: 'doctype',
    name: '',
    description: ''
  };

  S.sender('new-doctype-built', JSON.stringify(obj));

  return 'doctype-sent-to-editor';
};

exports.init = init;
exports.get = get;
exports.prefix = prefix;
exports.addDoctype = addDoctype;

},{"../sender.js":97,"node-uuid":51,"pager":143,"templates":52}],60:[function(require,module,exports){
// # Config Editor
//
// *Implicit depends:* DOM
//
// All code for working with the editor.

// ## Variable Definitions

var formalize = require('formalize');
var ajax = require('ajax');
var sess = require('sess');
var S = require('../sender.js');
var toggle;
var init;

// ## Internal Functions

// Toggle collapsed class
var toggleCollapseNode = function (node) {
  'use strict';

  node.classList.toggle('collapsed');

  return node;
};

// Toggle hide class
var toggleHideNode = function (node) {
  'use strict';

  node.classList.toggle('hidden');

  return node;
};

// Providing a shorter name to call this function.
var forEach = function (list, fun) {
  'use strict';

  Array.prototype.forEach.call(list, fun);
};

// Update the default attribute when the value property changes.
var updateDefaults = function (e) {
  'use strict';

  var t = e.target;
  var val = t.value;

  if (t.type === 'text') {
    t.setAttribute('value', val);
  }

  return t;
};

// Get the editor form object.
var editForm = function () {
  'use strict';

  return document.getElementById('edit-form');
};

// Update the attributes associated with the text of the label.
var updateLabelAttributes = function (e) {
  'use strict';

  var label = e.target;
  var elem = document.getElementById(label.nextSibling.id);

  label.title = label.textContent;

  if (label.classList.contains('span-title')) {
    elem.title = label.textContent;
  } else {
    elem.name = label.textContent;
  }
};

// Initialize the form labels.
var formLabelsInit = function (form) {
  'use strict';

  forEach(form.getElementsByTagName('span'), function (item) {
    if (!item.classList.contains('array-element-handle')) {
      item.contentEditable = true;
      item.oninput = updateLabelAttributes;
    }
  });
};

// Initialize the form inputs.
var formInputsInit = function (form) {
  'use strict';

  forEach(form.getElementsByTagName('input'), function (item) {
    item.onchange = updateDefaults;
  });
};

// Remove the class from all instances.
var removeClass = function (item, className) {
  'use strict';

  if (item) {
    item.classList.remove(className);
  }

  return item;
};

// Get previous mark.
var getMark = function () {
  'use strict';

  return {
    mark: document.getElementsByClassName('marked')[0],
    line: document.getElementsByClassName('marked-line')[0]
  };
};

// Remove previous mark.
var removeMark = function () {
  'use strict';

  var old = getMark();
  removeClass(old.mark, 'marked');
  removeClass(old.line, 'marked-line');

  return old;
};

var addMark = function (line, item) {
  'use strict';

  removeMark();
  line.classList.add('marked-line');
  item.classList.add('marked');

  return getMark();
};

// Keep track of last element with focus.
var markItem = function (item) {
  'use strict';

  return function (e) {
    addMark(item, e.target);
  };
};

// Initialize form elements.
var formElementsInit = function (form) {
  'use strict';

  forEach(form.getElementsByTagName('li'), function (item) {
    forEach(item.children, function (child) {
      child.onfocus = markItem(item);
    });
  });
};

// Initialize the form.
var formInit = function (form) {
  'use strict';

  formInputsInit(form);
  formElementsInit(form);
  formLabelsInit(form);
};

// Set the default options.
var setDefaultOptions = function (options) {
  'use strict';
  options = options ? options : {};
  options.spanLabel = true;

  return options;
};

// Given some json, create a form, perform initialization and display
// it in the editor area.
var fillForm = function (json, options) {
  'use strict';

  options = setDefaultOptions(options);
  var formHTML = formalize.toForm(json, options);
  var form = editForm();

  form.innerHTML = formHTML;
  formInit(form);

  return 'form-filled';
};

// Is the element an unordered list element?
var isHTMLUList = function (elem) {
  'use strict';

  return elem instanceof HTMLUListElement;
};

// Is the element an ordered list element?
var isHTMLOList = function (elem) {
  'use strict';

  return elem instanceof HTMLOListElement;
};

// Is the element a list element?
var isHTMLList = function (elem) {
  'use strict';

  return isHTMLOList(elem) || isHTMLUList(elem);
};

// Is this the child of a `ul` list?
var isChildOfHTMLOLList = function (elem) {
  'use strict';

  return isHTMLOList(elem.parentElement);
};

// Array elements don't have labels.
var maybeRemoveLabel = function (elem, targ) {
  'use strict';

  if (targ && isHTMLOList(targ)) {
    elem.removeChild(elem.getElementsByTagName('span')[0]);
    elem.firstChild.removeAttribute('name');
    elem.firstChild.removeAttribute('title');
  }

  return elem;
};

// Get the last child of this node.
var getLastChild = function (node) {
  'use strict';

  return Array.prototype.slice.call(node.children, -1)[0];
};

// Predicate function to determine if item is a UL or OL.
var isLineHTMLList = function (targ) {
  'use strict';

  var lastChild = getLastChild(targ);

  return isHTMLList(lastChild);
};

// Find the target placement for a new element and return a function
// that will place it there.
var getInserter = function (targ, asChild) {
  'use strict';

  var insertPoint = targ.parentNode;
  var retval;

  if (targ) {
    // When the item should be added as a child to another item.
    if (asChild) {
      if (isLineHTMLList(targ)) {
        insertPoint = getLastChild(targ);
      } else {
        // This is the wrong type of target element for adding a child
        // to.
        asChild = false;
      }
    }

    retval = function (elem) {
      elem = maybeRemoveLabel(elem, insertPoint);

      if (targ.nextSibling && !asChild) {
        insertPoint.insertBefore(elem, targ.nextSibling);
      } else {
        insertPoint.appendChild(elem);
      }

      return elem;
    };
  } else {
    retval = function (elem) {
      elem = maybeRemoveLabel(elem, targ);

      var firstObj = editForm().getElementsByTagName('ul')[0];
      firstObj.appendChild(elem);

      return elem;
    };
  }

  return retval;
};

// Add an element given JSON.
var addElement = function (targ, json, asChild) {
  'use strict';

  var tmp = document.createElement('div');
  var tmpForm = formalize.toForm(json, setDefaultOptions());
  var inserter = getInserter(targ, asChild);
  var newElem;

  tmp.innerHTML = tmpForm;
  formInit(tmp);
  newElem = tmp.getElementsByTagName('li')[0];
  return inserter(newElem);
};

var defaultToggle = function () {
  'use strict';

  forEach(document.querySelectorAll('#edit-form span.span-title'), function (x) {
    toggle('title', x);
  });
  forEach(document.querySelectorAll('#edit-form ol > li'), function (x) {
    toggle('array-elem', x);
  });

  return 'default-toggle-applied';
};

// Internal version of `elementDelete()`, which takes an argument instead
// of using the marked line and returns the outer HTML of the deleted
// element/node.
var internalElementDelete = function (targ) {
  'use strict';

  var html = targ.outerHTML;

  targ.parentElement.removeChild(targ);

  return html;
};

// Internal version of `addObjectElement()`, which takes an arguement
// instead of using the marked line and returns the HTML element added
// instead of a message.
var internalAddObjectElement = function (targ, asChild) {
  'use strict';

  return addElement(targ, '{"_blank_":{"_first_":""}}', asChild);
};

// Internal version of `copy()`, which takes an arguement instead of
// using the marked line and  returns the node that was copied.
var internalCopy = function (targ) {
  'use strict';

  var copyInfo = {
    _id: 'shimi-ima-copied',
    html: targ.outerHTML,
    parentWasOL: isChildOfHTMLOLList(targ)
  };

  sess.replace(copyInfo);

  return targ;
};

// Internal version of `cut()`, which takes an arguement instead of
// using the marked line and  returns the HTML that was copied.
var internalCut = function (targ) {
  'use strict';

  var copiedElement = internalCopy(targ);

  return internalElementDelete(copiedElement);
};

// Internal version of `paste()` which takes an arguement instead of
// using the marked line and returns the pasted node. The copied element
// is also provided by an argument instead of by using sessionStorage.
var internalPaste = function (targ, copied, asChild) {
  'use strict';

  var tmp = document.createElement('div');
  var tmpForm = document.createElement('form');
  var tmpWrap = document.createElement('ul');
  var copiedChild;
  var json;

  tmpWrap.innerHTML = copied.html;

  if (copied.parentWasOL) {
    copiedChild = tmpWrap.firstChild.firstChild;

    if (isHTMLList(copiedChild)) {
      copiedChild.setAttribute('title', '_blank_');
    } else {
      copiedChild.setAttribute('name', '_blank_');
    }
  }

  tmpForm.appendChild(tmpWrap);
  tmp.appendChild(tmpForm);

  json = formalize.fromForm(tmp.innerHTML);

  return addElement(targ, json, asChild);
};

// ## Exported Functions

// Get the specified stored document and load it into the editor.
var get = function (url) {
  'use strict';

  var complete = function (req) {
    return init(JSON.stringify(req.response));
  };

  ajax.get(url, complete);

  return 'object-loaded';
};

// Load an empty object into the editor.
var fresh = function () {
  'use strict';

  fillForm('{}');

  return 'empty-object-loaded';
};

var create = function () {
  'use strict';

  var form = editForm();
  var json = formalize.fromForm(form.innerHTML);
  var obj = JSON.parse(json);
  var category = obj.category;
  var complete = function () {
    S.sender('config-' + category + '-created');
  };

  ajax.post('config/' + category + 's', json, complete);

  return 'object-created';
};

var update = function (args) {
  'use strict';

  var form = editForm();
  var json = formalize.fromForm(form.innerHTML);
  var obj = JSON.parse(json);
  var category = obj.category;
  var identifier = obj._id;
  var revision = obj._rev;
  var url = 'config/' + category + 's/' + identifier + '?rev=' + revision;
  var complete = function () {
    get('doctypes/' + obj._id);

    S.sender('config-' + category + '-updated');
  };

  ajax.put(url, json, complete);

  return 'object-updated';
};

var remove = function (args) {
  'use strict';

  var answer = window.confirm('Are you sure you want to delete this?');
  var form = editForm();
  var json = formalize.fromForm(form.innerHTML);
  var obj = JSON.parse(json);
  var category = obj.category;
  var identifier = obj._id;
  var revision = obj._rev;
  var url = 'config/' + category + 's/' + identifier + '?rev=' + revision;
  var complete = function () {
    S.sender('config-' + category + '-deleted');
  };

  if (answer) {
    ajax.del(url, complete);
  }

  return 'object-removed';
};

var restore = function (args) {
  'use strict';

  return 'object-restored';
};

// Move and element up in the tree.
var elementUp = function () {
  'use strict';

  var targ = getMark().line;
  var prev = targ.previousSibling;

  if (prev) {
    targ.parentElement.insertBefore(targ, prev);
  }

  return 'element-moved-up';
};

// Move and element down in the tree.
var elementDown = function () {
  'use strict';

  var targ = getMark().line;
  var next = targ.nextSibling;

  if (next) {
    targ.parentElement.insertBefore(targ, next.nextSibling);
  } else {
    targ.parentElement.appendChild(targ);
  }

  return 'element-moved-down';
};

// Remove the marked element from the tree.
var elementDelete = function () {
  'use strict';

  var targ = getMark().line;

  internalElementDelete(targ);

  return 'element-removed';
};

// Add an object element to the form.
var addObjectElement = function (asChild) {
  'use strict';

  var targ = getMark().line;

  internalAddObjectElement(targ, asChild);

  return 'object-element-added';
};

// Add an array element to the form.
var addArrayElement = function (asChild) {
  'use strict';

  var targ = getMark().line;

  addElement(targ, '{"_blank_":[""]}', asChild);

  return 'array-element-added';
};

// Add a text element to the form.
var addTextElement = function (asChild) {
  'use strict';

  var targ = getMark().line;

  addElement(targ, '{"_blank_":""}', asChild);

  return 'text-element-added';
};

// Add an object element to the form.
var addChildObjectElement = function () {
  'use strict';

  addObjectElement(true);

  return 'child-object-element-added';
};

// Add a child array element to an object or array.
var addChildArrayElement = function () {
  'use strict';

  addArrayElement(true);

  return 'child-array-element-added';
};

// Add a child text element to an object or array.
var addChildTextElement = function () {
  'use strict';

  addTextElement(true);

  return 'child-text-element-added';
};

// Mark a "line" aka an entire li. This is invoked when ol > li is
// clicked.
var markLine = function (line) {
  'use strict';

  addMark(line, line);

  return 'line-marked';
};

// Toggle the visibility of a group.
toggle = function (kind, node) {
  'use strict';

  var hideNode = false;

  if (kind === 'title') {
    hideNode = node.nextSibling;
  } else if (isHTMLList(node.children[0])) {
    hideNode = node.children[0];
  }

  if (hideNode) {
    toggleCollapseNode(node);
    toggleHideNode(hideNode);
  }

  return 'toggled-subgroup';
};

// Paste a node plus children in new context.
var paste = function (asChild) {
  'use strict';

  var targ = getMark().line;
  var copied = sess.get('shimi-ima-copied');

  if (copied !== null) {
    internalPaste(targ, copied, asChild);
  }

  return 'pasted';
};

// Paste a node plus children in new context as a child of the currently
// marked element.
var pasteChild = function () {
  'use strict';

  paste(true);

  return 'child-pasted';
};

// Copy the marked item into session storage
var copy = function () {
  'use strict';

  var markedLine = getMark().line;

  internalCopy(markedLine);

  return 'copied';
};

// Copy the marked item into session storage before deleting it.
var cut = function () {
  'use strict';

  var markedLine = getMark().line;

  internalCut(markedLine);

  return 'cut';
};

// Move marked to a higher tier.
var promote = function () {
  'use strict';

  return 'promoted';
};

// Move marked to a lower obj/UL tier.
var demote = function () {
  'use strict';

  var targ = getMark().line;
  var oldCopied = sess.get('shimi-ima-copied');
  var newObject = internalAddObjectElement(targ);
  var newCopied;
  var newElem;

  internalCut(targ);
  newCopied = sess.get('shimi-ima-copied');
  newElem = internalPaste(newObject, newCopied, true);
  internalElementDelete(newElem.previousSibling);
  addMark(newElem, newElem.firstChild);
  sess.replace(oldCopied);

  return 'demoted';
};

// Initialize the editor, loading a fresh object.
init = function (json) {
  'use strict';

  if (json) {
    fillForm(json);
    defaultToggle();
  } else {
    fresh();
  }

  return 'editor-initialized';
};

exports.init = init;
exports.get = get;
exports.fresh = fresh;
exports.update = update;
exports.create = create;
exports.remove = remove;
exports.restore = restore;
exports.toggle = toggle;
exports.elementUp = elementUp;
exports.elementDown = elementDown;
exports.elementDelete = elementDelete;
exports.addObjectElement = addObjectElement;
exports.addArrayElement = addArrayElement;
exports.addTextElement = addTextElement;
exports.addChildObjectElement = addChildObjectElement;
exports.addChildArrayElement = addChildArrayElement;
exports.addChildTextElement = addChildTextElement;
exports.markLine = markLine;
exports.copy = copy;
exports.cut = cut;
exports.paste = paste;
exports.pasteChild = pasteChild;
exports.promote = promote;
exports.demote = demote;

},{"../sender.js":97,"ajax":105,"formalize":130,"sess":147}],61:[function(require,module,exports){
// # Maintenance User Interface
//
// *Implicit depends:* DOM
//
// This handles UI elements that are used for maintaining a project.

// ## Variable Definitions

var templates = require('templates');
var ajax = require('ajax');
var flash = require('flash');

// ## Exported Functions

// When the upgrade button is pressed in the configuration UI, this
// will carry out the necessary action. It will make an empty `POST`
// to the upgrade path and alert the user that this was done.
var upgradeButton = function () {
  'use strict';

  ajax.post('config/upgrade', false, function () {
    flash.highlight('Task Started', 'Upgrade Project');
  });

  return 'upgrade-initiated';
};

// Initialize and display the interface.
var init = function () {
  'use strict';

  var renderedHTML = templates['config-maintenance']();
  document.getElementById('config-maintenance').insertAdjacentHTML('beforeend', renderedHTML);

  return 'maintenanceui-initialized';
};

exports.init = init;
exports.upgradeButton = upgradeButton;

},{"ajax":105,"flash":128,"templates":52}],62:[function(require,module,exports){
// # Dispatching double click events
//
// *Implicit depends:* DOM, JQuery, JQueryUI
//
// Almost all double click events that are handled by the system are
// listed here. With [sender.js](./sender.html) and other dispatchers,
// the hope is to centralize effects and interdependencies. At some
// point I may use a more sophisticated approach.

// ## Variable Definitions

var dispatcher = require('dispatcher').dispatcher;
var panelToggler = require('panel-toggle').panelToggler;
var searchui = require('documents/searchui');
var ceditui = require('config/editui');
var worksheetui = require('documents/worksheetui');

// ## Exported Functions

// Given a click event, determine what action to take based on the
// click target.
var dblclickDispatch = function (e) {
  'use strict';

  var action = dispatcher({
    '#edit-form span.span-title': function (t) {
      ceditui.toggle('title', t);
    },
    '#edit-form ol > li': function (t) {
      ceditui.toggle('array-elem', t);
    },
    '.search-result-field-id a': function (t) {
      searchui.addField($(t).parent('h5'));
    },
    '.field-view b': function (t) {
      searchui.addField($(t).parent('li'));
    },
    '.field-container label span': function (t) {
      searchui.addField($(t).parent('label').parent('div'));
    },
    '#index-index-input-label': function () {
      searchui.addIndex();
    },
    '.panel > h2': function (t) {
      panelToggler(t);
    },
    '#toggle-handles': function (t) {
      worksheetui.hideHandles();
    },
    '.fieldset-handle': function (t) {
      worksheetui.hideFieldset($(t).attr('data-field-fieldset'));
    },
    '.field-handle': function (t) {
      worksheetui.hideField($(t).attr('data-field-field'));
    }
  });

  action(e);
};

exports.dblclickDispatch = dblclickDispatch;

},{"config/editui":111,"dispatcher":114,"documents/searchui":122,"documents/worksheetui":126,"panel-toggle":144}],63:[function(require,module,exports){
// # Dispatcher for clicks and double clicks
//
// *Implicit depends:* DOM
//
// See [`click-dispatch.js`](./click-dispatch.html) and
// [`dblclick-dispatch.js`](./dblclick-dispatch.html).

// # Exported Functions

// Match the target to a pattern and run its action.
var dispatcher = function (patterns) {
  'use strict';

  var d = function (e) {
    var target = e.target;

    Object.keys(patterns).forEach(function (pattern) {
      if (target.matches(pattern)) {
        var action = patterns[pattern];
        action(target);
      }
    });
  };

  return d;
};

exports.dispatcher = dispatcher;

},{}],64:[function(require,module,exports){
// # Paging For Changes Listing
//
// *Implicit depends:* DOM, JSON
//
// Loads changes based on user suplied values.

// Variable Definitions

var pager = require('pager').pager;
var info = require('documents/information');

// Exported Functions

// Return the 'prefix' which is used in id and class names for
// elements used to page through these values.
var prefix = function () {
  'use strict';

  return 'changelog';
};

// Called by a keystroke event handler when user changes form values.
var get = function () {
  'use strict';

  var url = prefix();
  var target = document.getElementById(prefix() + '-listing');

  var format = function (resp) {
    resp.rows.map(function (item) {
      if (item.doc.changes) {
        item.doc.changes = Object.keys(item.doc.changes).map(function (key) {
          return item.doc.changes[key];
        });
      }
    });

    return resp;
  };

  var filterMod = function (filterVal) {
    var retval = info.doctypeId() + '-';

    if (filterVal && filterVal !== '') {
      retval = retval + filterVal.replace(/\D/, '');
    }

    return retval;
  };

  pager({
    prefix: prefix(),
    url: url,
    format: format,
    target: target,
    filterMod: filterMod
  }).get();

  return true;
};

exports.prefix = prefix;
exports.get = get;

},{"documents/information":121,"pager":143}],65:[function(require,module,exports){
// # Keyboard shortcuts
//
// *Implicit depends:* DOM, JQuery
//
// Handles the input area and command execution. Keyboard events are
// handled in [keystrokes.js](./keystrokes.html).

// Variable Definitions

var editui = require('documents/editui');
var S = require('../sender.js');

// Internal functions

var commandInput = function () {
  'use strict';

  return document.getElementById('edit-command-input');
};

var commandDialog = function () {
  'use strict';

  return $('#command-dialog');
};

var setContext = function (elem, context) {
  'use strict';

  return elem.attr('data-last-active', context);
};

var getContext = function (elem) {
  'use strict';

  return elem.attr('data-last-active');
};

// Exported functions

// Lookup the command and perform an action.
var execute = function (command) {
  'use strict';

  var restoreFocus = true;

  switch (command) {
  case 'w':
  case 'clear':
    editui.clear();
    break;
  case 'c':
  case 'create':
    editui.create();
    restoreFocus = false;
    break;
  case 's':
  case 'save':
    editui.save();
    break;
  case 'd':
  case 'delete':
    $('#document-view').show();
    if ($('#document-delete-button').css('display') !== 'none') {
      $('#document-delete-button').click();
    }
    break;
  case 'e':
  case 'edit':
    $('#document-view').show();
    if ($('#document-edit-button').css('display') !== 'none') {
      $('#document-edit-button').click();
      restoreFocus = false;
    }
    break;
  case 'r':
  case 'restore':
    $('#document-view').show();
    if ($('#document-restore-button').css('display') !== 'none') {
      $('#document-restore-button').click();
    }
    break;
  }

  if (restoreFocus) {
    var cdialog = commandDialog();
    var context = getContext(cdialog);
    $('#' + context).focus();
  } else {
    S.sender('lost-focus');
  }

  S.sender('executed-command');
  return true;
};

// Open the command dialog
var dialogOpen = function (context) {
  'use strict';

  var cinput = commandInput();
  var cdialog = commandDialog();
  cinput.value = '';
  setContext(cdialog, context).show();
  cinput.focus();
  return true;
};

// Close the command dialog
var dialogClose = function () {
  'use strict';

  var cinput = commandInput();
  var cdialog = commandDialog();
  setContext(cdialog, '').hide();
  cinput.value = '';
  return true;
};

exports.execute = execute;
exports.dialogOpen = dialogOpen;
exports.dialogClose = dialogClose;

},{"../sender.js":97,"documents/editui":118}],66:[function(require,module,exports){
// # Documents sub-application
//
// *Implicit depends:* DOM, JQuery
//
// Shared document editing stuff plus initialization.

// ## Variable Definitions

var ui = require('documents/ui-shared');
var info = require('documents/information');
var setsui = require('documents/setsui');
var editui = require('./editui.js');
var viewui = require('documents/viewui');
var indexui = require('documents/indexui');
var changeui = require('documents/changeui');
var S = require('../sender.js');
var ajax = require('ajax');
var identifier;

// ## Internal functions

// In practice this is the select listing of the user created indexes
// which is triggering the change event.
//
// *TODO* put this with other change handlers.
var indexForm = function () {
  'use strict';

  // TODO Remove JQuery
  $('#index-filter-form select').change(function () {
    indexui.get();
  });

  return true;
};

// If there is a hash at the end of the URL with a document ID specified,
// this will pass the information on the correct funciont in `viewui`.
var loadHash = function (urlHash) {
  'use strict';

  if (urlHash) {
    viewui.get(urlHash);
  }

  return true;
};

// ## Exported functions

// Initialize the documents sub-application.
var init = function () {
  'use strict';

  // TODO: there should be a better place for this.
  document.onsubmit = function () {
    return false;
  };

  S.sender('document-init-stage-1');

  return true;
};

// Initialization dependent on init.
var init2 = function () {
  'use strict';

  setsui.updateSelection();
  indexui.iOpts();
  indexui.get();
  indexForm();
  editui.init();
  loadHash(window.location.hash.split('#')[1]);
  changeui.get();
};

exports.init = init;
exports.init2 = init2;

},{"../sender.js":97,"./editui.js":67,"ajax":105,"documents/changeui":115,"documents/indexui":120,"documents/information":121,"documents/setsui":123,"documents/ui-shared":124,"documents/viewui":125}],67:[function(require,module,exports){
// # Documents sub-application
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Edit pane UI elements

// Variable Definitions

var templates = require('templates');
var store = require('store').store;
var form = require('form');
var flash = require('flash');
var ajax = require('ajax');
var fieldsets = require('./fieldsets.js');
var viewui = require('documents/viewui');
var indexui = require('documents/indexui');
var info = require('documents/information');
var ui = require('documents/ui-shared');
var uuid = require('node-uuid');
var afterRefresh;
var setInstanceInfo;

// Internal functions

// Get the fieldset id for a field id.
var getFieldsetId = function (fieldId) {
  'use strict';

  var lookup = JSON.parse(sessionStorage.getItem(ui.identifier() + '_fieldsToFieldset'));

  return lookup[fieldId];
};

// Display validation error properly.
var validationError = function (req) {
  'use strict';

  var body = JSON.parse(req.responseText);
  var title = req.statusText;

  var invalid = document.querySelector('[data-field-instance="' + body.instance + '"]');
  var invalidTab = document.querySelector('[href="#' + getFieldsetId(invalid.dataset.fieldField) + '"]').parentElement;

  invalidTab.classList.add('ui-state-error');
  invalid.classList.add('ui-state-error');

  flash.error(title, body.fieldname + ' ' + body.message);

  return true;
};

// The expander for textareas may need the proper information set for
// multiple fieldsets
var setExpander = function (item) {
  'use strict';

  var expander = item.parentElement.querySelector('.expander');

  if (expander) {
    expander.dataset.groupId = item.id;
  }

  return true;
};

// Fields need to have instances. This should ensure they have them.
var instances = function (addInstances) {
  'use strict';

  var makeInstance = function () {
    return uuid.v4().replace(/-/g, '');
  };

  Array.prototype.forEach.call(document.querySelectorAll('#last-added [data-field-instance]'), function (item) {
    if (!item.dataset.fieldInstance || item.dataset.fieldInstance.length === '') {
      var instance = makeInstance();

      item.dataset.fieldInstance = instance;
      setInstanceInfo(item);
    }
  });

  return true;
};

// Exported functions

// Initialize the editing pane.
// TODO: refactor taking advantage of information.info(). Old code used
// ajax calls and server rendered HTML.
var init = function () {
  'use strict';

  var fs = {};
  var editArea = document.getElementById('document-edit');

  fs.fieldsets = info.info().fieldsets;
  fs.has_rows = fs.fieldsets ? (fs.fieldsets.length > 0) : false;
  editArea.innerHTML = templates['document-edit'](fs);
  // TODO: replace tabs functionality.
  $('#edit-tabs').tabs();
  fieldsets.initFieldsets();

  return true;
};

// Focus on the first focusable input element in an active tab.
var selectInput = function () {
  'use strict';

  var inputable = 'input, select, textarea';
  var curId = document.querySelector('.ui-tabs-active a').getAttribute('href').slice(1, 33);

  document.getElementById(curId).querySelector(inputable).focus();

  return true;
};

// Used as a variation of `afterRefresh` where a boolean is provided
// to specify if new instances identifiers should be created and
// set. Basically this is for a completely fresh refresh, when the form
// is in the state such that a document can be created but no information
// is available to do an update.
var afterFreshRefresh = function (addInstances) {
  'use strict';

  afterRefresh(addInstances);

  return true;
};

// Run after the edit button in the view UI is clicked.
var afterEditRefresh = function () {
  'use strict';

  var sharedAttrs = ['data-document-document', 'data-document-rev'];

  sharedAttrs.forEach(function (elem) {
    ui.saveButton().setAttribute(elem, ui.viewInfo().getAttribute(elem));
  });

  ui.showEnable(ui.saveButton());
  afterRefresh();

  return true;
};

// Essentially initialization of the form. If `addInstances` is true,
// new instance identifiers will be created for a blank form.
afterRefresh = function (addInstances) {
  'use strict';

  instances(addInstances);
  form.initDateFields();

  return true;
};

// Remove a class from some items.
var clearErrorStates = function () {
  'use strict';

  Array.prototype.forEach.call(ui.editForm().querySelectorAll('.ui-state-error'), function (item) {
    item.classList.remove('ui-state-error');
  });

  return true;
};

// Remove all the fields.
var removeFields = function () {
  'use strict';

  Array.prototype.forEach.call(document.querySelectorAll('.fields'), function (item) {
    item.parentNode.removeChild(item);
  });
};

// Combine two shallow objects.
var extend = function (oldO, newO) {
  'use strict';

  Array.prototype.forEach.call(Object.keys(newO), function (key) {
    oldO[key] = newO[key];
  });

  return oldO;
};

// To be run if the user chooses to save the form contents. This is an
// update, not creation.
var save = function () {
  'use strict';

  if (ui.saveButton().classList.contains('oldrev')) {
    if (!window.confirm('This data is from an older version of this document. Are you sure you want to restore it?')) {
      return false;
    }
  }

  var sb = ui.saveButton();
  var s = store(sb);
  var doc = s.d('document');
  var rev = s.d('rev');
  var url = './documents/' + doc + '?rev=' + rev;
  var firstIndex = document.getElementById('first-index-element');
  var newObj;
  var obj = {
    doctype: s.d('doctype'),
    description: s.d('description')
  };
  var statusCallbacks = [];
  var success = function (req) {
    viewui.get(doc);
    indexui.get(ui.skey(), ui.sid());
    flash.highlight('Success', 'Your document was saved.');
    sb.classList.remove('oldrev');
    sb.dataset.documentRev = req.response.rev;
    ui.showEnable(sb);
  };
  statusCallbacks[204] = success;
  statusCallbacks[200] = success;
  statusCallbacks[403] = function (req) {
    validationError(req);
    ui.showEnable(sb);
  };
  statusCallbacks[409] = function (req) {
    flash.error(req.statusText, req.response.message);
    ui.hideDisable(sb);
  };

  clearErrorStates();
  ui.hideDisable(ui.saveButton());
  newObj = fieldsets.fieldsetsToObject(ui.editForm());
  obj = extend(obj, newObj);
  ajax.put(url, obj, undefined, statusCallbacks);
};

// To be run if creating a new document.
var create = function () {
  'use strict';

  var s = store(ui.createButton());
  var url = 'documents';
  var newObj;
  var obj = {
    doctype: s.d('doctype'),
    description: s.d('description')
  };
  var statusCallbacks = [];
  statusCallbacks[201] = function (req) {
    var title = 'Success';
    var body = 'Your document was created.';
    var documentId = req.getResponseHeader('Location').match(/[a-z0-9]*$/);

    ui.hideDisable(ui.saveButton());
    removeFields();
    fieldsets.initFieldsets();
    viewui.get(documentId);
    indexui.get(ui.skey(), ui.sid());
    flash.highlight(title, body);
    ui.showEnable(ui.createButton());
  };
  statusCallbacks[403] = function (req) {
    validationError(req);
    ui.showEnable(ui.createButton());
  };

  clearErrorStates();
  ui.hideDisable(ui.createButton());
  newObj = fieldsets.fieldsetsToObject(ui.editForm());
  obj = extend(obj, newObj);
  ajax.post(url, obj, undefined, statusCallbacks);
};

// Clear the form.
var clear = function () {
  'use strict';

  clearErrorStates();
  ui.hideDisable(ui.saveButton());
  removeFields();
  fieldsets.initFieldsets();
};

// Display a help dialog for a form field.
var showHelpDialog = function (target) {
  'use strict';

  if (target.classList.contains('.label-text')) {
    target = target.parentElement.querySelector('.ui-icon-help');
  }

  // TODO: remove this JQuery UI dependency
  $('#help-dialog').dialog().dialog('open').find('#help-dialog-text').html(target.getAttribute('title'));

  return true;
};

// Contract and expand textarea elements.
var toggleTextarea = function (target) {
  'use strict';

  var textarea = document.getElementById(target.dataset.groupId);

  if (target.id === textarea.dataset.groupId) {
    // This is the key sequence case.
    textarea.classList.toggle('expanded');
    textarea.parentElement.querySelector('span.expander').classList.toggle('expanded');
  } else {
    // This is the click case.
    textarea.classList.toggle('expanded');
    target.classList.toggle('expanded');
  }

  return true;
};

// When the item has an instance, the id and group id must be reset.
setInstanceInfo = function (item) {
  'use strict';

  item.id = item.dataset.fieldField + '-' + item.dataset.fieldInstance;
  item.dataset.groupId = item.id;
  setExpander(item);
};

exports.init = init;
exports.selectInput = selectInput;
exports.afterFreshRefresh = afterFreshRefresh;
exports.afterEditRefresh = afterEditRefresh;
exports.afterRefresh = afterRefresh;
exports.save = save;
exports.create = create;
exports.clear = clear;
exports.toggleTextarea = toggleTextarea;
exports.setInstanceInfo = setInstanceInfo;
exports.showHelpDialog = showHelpDialog;

},{"./fieldsets.js":68,"ajax":105,"documents/indexui":120,"documents/information":121,"documents/ui-shared":124,"documents/viewui":125,"flash":128,"form":129,"node-uuid":51,"store":149,"templates":52}],68:[function(require,module,exports){
// # Fieldsets (and fields)
//
// *Implicit depends:* DOM
//
// Dealing with fields and fieldsets.

// Variable Definitions

var path = require('../path.js').path;
var store = require('store').store;
var utils = require('utils');
var editui = require('./editui.js');
var info = require('documents/information');
var ui = require('documents/ui-shared');
var ajax = require('ajax');
var templates = require('templates');
var dateOrNumber;
var getEncoded;
var getFieldValue;
var fillFields;
var setFieldValue;
var initFieldset;

// Internal functions

// Get the container for a fieldset with `id`.
var fsContainer = function (id) {
  'use strict';

  return document.getElementById('container-' + id);
};

// Get the doctype path.
var dpath = function (source, category) {
  'use strict';

  var url = path(source, category);
  url.doctype = false;
  return url;
};

// If the item referred to by `key` is in session storage perform the
// `success` action with the stored items as the argument, otherwise,
// get the item from the document info and render it.
// WARNING: This exists as part of a minimal change to support a very
// different way of managing this information. It will be rewritten
// soon.
var ifStoredElse = function (key, success, otherwise) {
  'use strict';

  var item;
  var id = key.replace(/^fieldsets\/([^/]*)(\/fields)*$/, '$1');
  var fieldset;

  item = sessionStorage.getItem(key);

  if (item) {
    success(item);
  } else {
    fieldset = info.info().fieldsets.filter(function (x) {
      return x._id === id;
    })[0];

    otherwise(fieldset);
  }
};

// Get the allowed file values from the server.
var getFileAllowed = function (field, callback) {
  'use strict';

  // This is unimplemented.
  return function () {
    callback();
  };
};

// Get the allowed doc values from the server.
var getAllowed = function (field, callback) {
  'use strict';

  var url = '/projects/project-' + info.project() + '/doctypes/' + field.source + '/documents/index';

  return function () {
    ajax.get(url, function (req) {
      var rows = req.response.rows;

      field.allowed = rows.length > 0 ? rows.map(function (x) {
        var value = x.key.map(function (y) {
          return y[1];
        }).join(', ');

        return {
          value: value,
          is_default: value === field['default']
        };
      }) : null;

      return callback();
    });
  };
};

// Process the listing of allowed values.
var processAllowed = function (field, callback) {
  'use strict';

  if (!field.allowed || field.allowed.length === 0) {
    field.allowed = null;
  } else {
    field.allowed = field.allowed.map(function (x) {
      return {
        value: x,
        is_default: x === field['default']
      };
    });
  }

  return function () {
    callback();
  };
};

// Process the fields before applying the template.
var processFields = function (fieldset, callback) {
  'use strict';

  var fields = fieldset.fields;
  var combined;

  combined = fields.reduce(function (acc, field) {
    var retval = acc;

    field.default_exists = field['default'] === '' ? false : field['default'];
    field.is_null = field['default'] === null;
    field.is_false = field['default'] === false;
    field[field.subcategory] = field.subcategory === field.subcategory;

    if (field.docselect || field.docmultiselect) {
      retval = getAllowed(field, acc);
    } else if (field.file) {
      retval = getFileAllowed(field, acc);
    } else if (field.allowed) {
      retval = processAllowed(field, acc);
    }

    return retval;
  }, function () {
    return callback(fieldset);
  });

  combined();

  return fieldset;
};

// Convert field values to an object that can be converted to JSON
var fieldsToObject = function (fields, index) {
  'use strict';

  fields = fields.querySelectorAll('.field-container .field');
  var obj = {
    fields: []
  };

  Array.prototype.forEach.call(fields, function (field, i) {
    var s = store(field);
    var value = getFieldValue(field);
    var instance = s.f('instance');

    obj.fields[i] = {
      id: s.f('field'),
      name: s.f('name'),
      label: s.f('label'),
      head: s.f('head') === 'true',
      reversal: s.f('reversal') === 'true',
      required: s.f('required') === 'true',
      min: dateOrNumber(s.f('subcategory'), s.f('min')),
      max: dateOrNumber(s.f('subcategory'), s.f('max')),
      instance: instance,
      charseq: s.f('charseq'),
      regex: s.f('regex'),
      order: s.f('order') * 1,
      subcategory: s.f('subcategory'),
      value: value
    };

    if (index >= 0) {
      obj.fields[i].index = index;
    }
  });

  return obj;
};

// `min` and `max` are either dates or numbers. Provide the correct
// value or the correct type depending on the subcategory of the field.
dateOrNumber = function (subcategory, fieldvalue) {
  'use strict';

  if (subcategory === 'date') {
    return fieldvalue;
  } else {
    return utils.stringToNumber(fieldvalue);
  }
};

// Get the correct value for a boolean that can be null
var getOpenboolean = function (value) {
  'use strict';

  switch (value) {
  case 'true':
    value = true;
    break;
  case 'false':
    value = false;
    break;
  default:
    value = null;
  }

  return value;
};

// Get a number from a string. Blanks are returned as an empty string.
var getNumber = function (value) {
  'use strict';

  if (utils.isBlank(value)) {
    value = '';
  } else if (!isNaN(value)) {
    value = value * 1;
  }

  return value;
};

// Items in multiple select lists are URL encoded
var getMultiple = function (value) {
  'use strict';

  var retval;

  if (value && value.length > 0) {
    retval = Array.prototype.map.call(value, function (v) {
      return getEncoded(v.value);
    });
  } else {
    retval = null;
  }

  return retval;
};

// Items in select lists are URL encoded
getEncoded = function (value) {
  'use strict';

  return window.decodeURIComponent(value.replace(/\+/g, ' '));
};

// Get the value from a field using the subcategory to ensure
// that the value has the correct type and is properly formatted.
getFieldValue = function (field) {
  'use strict';

  var value;

  switch (store(field).f('subcategory')) {
  case 'boolean':
    value = field.checked;
    break;
  case 'openboolean':
    value = getOpenboolean(field.value);
    break;
  case 'integer':
  case 'rational':
    value = getNumber(field.value);
    break;
  case 'multiselect':
  case 'docmultiselect':
    value = getMultiple(field.selectedOptions);
    break;
  case 'select':
  case 'docselect':
    value = getEncoded(field.value);
    break;
  default:
    value = field.value;
  }

  return value;
};

// Basic initialization of fields.
var initFields = function (container, callback, addInstances) {
  'use strict';

  var url = dpath(container, 'field');
  var allFields = container.querySelectorAll('.fields');
  var section = allFields[allFields.length - 1];
  var prependIt = function (data) {
    if (addInstances) {
      section.id = 'last-added';
    }
    section.insertAdjacentHTML('afterbegin', data);
    if (callback) {
      callback(section);
    }

    editui.afterFreshRefresh(addInstances);
  };

  // This is an ugly bit of callback stuff. This is intended to be
  // rewritten soon.
  var storeIt = function (data) {
    processFields(data, function (processed) {
      var html = templates['fields'](processed);
      sessionStorage.setItem(url, html);
      prependIt(html);
    });
  };

  ifStoredElse(url.toString(), prependIt, storeIt);

  return true;
};

// Initialize and fill multifieldsets.
var fillMultiFieldsets = function (vfieldset) {
  'use strict';

  var id = store(vfieldset).fs('fieldset');
  var container = fsContainer(id);
  var url = dpath(vfieldset, 'fieldset');

  container.innerHtml = '';

  Array.prototype.forEach.call(document.querySelectorAll('.multifield'), function (multifield) {
    initFieldset(container, function (fieldset) {
      fillFields(multifield, fieldset);
    });
  });
};

// Initialize and fill normal fieldsets.
var fillNormalFieldsets = function (vfieldset) {
  'use strict';

  fillFields(vfieldset);
};

// Fill the fields with values taken from the view pane.
fillFields = function (container, context) {
  'use strict';

  Array.prototype.forEach.call(document.querySelectorAll('#edit-document-form .ui-state-error'), function (item) {
    item.removeClass('ui-state-error');
  });

  ui.showEnable(ui.saveButton());

  Array.prototype.forEach.call(document.querySelectorAll('.field-view'), function (item) {
    var valueJson = item.dataset.fieldValue;
    var id = item.dataset.fieldField;
    var instance = item.dataset.fieldInstance;
    var field;
    var value;

    // TODO: Here is where I could begin making all values be stored as JSON
    if (valueJson) {
      value = JSON.parse(valueJson);
    }

    if (!context) {
      context = document.body;
    }

    field = context.querySelector('.field[data-field-field="' + id + '"]');

    if (field) {
      setFieldValue(field, value, instance);
    }
  });

  return true;
};

// Properly set the value of the field.
setFieldValue = function (field, value, instance) {
  'use strict';

  if (field.classList.contains('boolean')) {
    field.checked = value;
  } else if (value && field.classList.contains('open-boolean')) {
    field.value = value.toString();
  } else {
    field.value = value;
  }

  if (instance && instance.length === 32) {
    field.dataset.fieldInstance = instance;

    editui.setInstanceInfo(field);
  }

  return true;
};

// Exported functions

// Initialize a fieldset.
initFieldset = function (fieldset, callback, addInstances) {
  'use strict';

  var url = dpath(fieldset, 'fieldset').toString();
  var id = store(fieldset).fs('fieldset');
  var container = fsContainer(id);
  var appendIt = function (data) {
    container.insertAdjacentHTML('beforeend', data);
    initFields(container, callback, addInstances);
  };
  var storeIt = function (data) {
    var html = templates['fieldset'](data);
    sessionStorage.setItem(url, html);
    appendIt(html);
  };

  ifStoredElse(url, appendIt, storeIt);

  return false;
};

// Before submitting the form, the form data is converted into an object
// that can be serialized to JSON. This begins with the fieldsets.
var fieldsetsToObject = function (root) {
  'use strict';

  var obj = {
    fieldsets: []
  };

  Array.prototype.forEach.call(root.getElementsByTagName('fieldset'), function (fieldset, i) {
    var s = store(fieldset);

    var fields;
    var newFsObj;

    var fsObj = {
      id: s.fs('fieldset'),
      multiple: s.fs('multiple') === 'true',
      collapse: s.fs('collapse') === 'true',
      name: s.fs('name'),
      label: s.fs('label'),
      order: s.fs('order') * 1
    };

    fields = fsContainer(fsObj.id).querySelectorAll('.fields');

    if (!fsObj.multiple) {
      newFsObj = fieldsToObject(fields[0]);
      Array.prototype.forEach.call(Object.keys(newFsObj), function (x) {
        fsObj[x] = newFsObj[x];
      });
    } else {
      fsObj.multifields = [];

      Array.prototype.forEach.call(fields, function (field, j) {
        fsObj.multifields[j] = fieldsToObject(field, j);
      });
    }

    obj.fieldsets[i] = fsObj;
  });

  return obj;
};

// Initialize fieldsets
var initFieldsets = function () {
  'use strict';

  Array.prototype.forEach.call(document.getElementsByTagName('fieldset'), function (fieldset, i) {
    var fs = store(fieldset);

    if (fs.fs('multiple') === 'false') {
      initFieldset(fieldset, false);
    }
  });

  return true;
};

// Remove a multifieldset. This is done after the remove button is
// pressed.
// TODO: Move to editui
var removeFieldset = function (target) {
  'use strict';

  target.parentNode.parentNode.removeChild(target.parentNode);
};

// Fill the fieldset with values from the view pane.
var fillFieldsets = function () {
  'use strict';

  Array.prototype.forEach.call(document.querySelectorAll('.fieldset-view'), function (fieldset) {
    if (store(fieldset).fs('multiple') === 'true') {
      fillMultiFieldsets(fieldset);
    } else {
      fillNormalFieldsets(fieldset);
    }
  });

  editui.afterEditRefresh();

  return true;
};

exports.initFieldset = initFieldset;
exports.fieldsetsToObject = fieldsetsToObject;
exports.initFieldsets = initFieldsets;
exports.removeFieldset = removeFieldset;
exports.fillFieldsets = fillFieldsets;

},{"../path.js":95,"./editui.js":67,"ajax":105,"documents/information":121,"documents/ui-shared":124,"store":149,"templates":52,"utils":150}],69:[function(require,module,exports){
// # Index Listing
//
// *Implicit depends:* DOM, JSON
//
// Loads index based on user suplied values. It also loads some other
// preliminary data, such as the listing of user created indexes. The
// `load()` function performs some initialization.

// ## Variable Definitions

var templates = require('templates');
var pager = require('pager').pager;
var ajax = require('ajax');
var ui = require('documents/ui-shared');
var viewui = require('./viewui.js');
var editui = require('./editui.js');

// ## Exported Functions

// Return the 'prefix' which is used in id and class names for
// elements used to page through these values.
var prefix = function () {
  'use strict';

  return 'index';
};

// Called by a keystroke event handler when user changes form values.
var get = function () {
  'use strict';

  var url = 'documents/' + prefix();
  var indexId = document.getElementById('index-' + prefix() + '-input').value;
  var target = document.getElementById(prefix() + '-listing');

  var format = function (resp) {
    resp.rows = resp.rows.map(function (item) {
      item.display_key = item.key.map(function (k) {
        return k[1];
      });

      if (indexId && item.value.length > 0) {
        item.value = item.value[1].split(', ');
      }

      return item;
    });

    return resp;
  };

  pager({
    prefix: prefix(),
    format: format,
    url: url,
    indexId: indexId,
    target: target
  }).get();

  return true;
};

// Loads the listing of user created indexes.
var iOpts = function () {
  'use strict';

  var url = 'indexes?as=options';
  var options;

  ajax.get(url, function (req) {
    var data = req.response;

    options = templates['index-options'](data);
    ui.indexIndexInput.innerHTML = options;
  });

  return true;
};

// This is the entry point that loads the data for this section of
// the application.
//
// TODO: Move to documents.js
var load = function (target) {
  'use strict';

  var id = target.getAttribute('href').slice(1);
  ui.dv.innerHTML = '<em>Loading...</em>';
  editui.clear();
  viewui.get(id);

  return true;
};

exports.prefix = prefix;
exports.get = get;
exports.iOpts = iOpts;
exports.load = load;

},{"./editui.js":67,"./viewui.js":74,"ajax":105,"documents/ui-shared":124,"pager":143,"templates":52}],70:[function(require,module,exports){
// # Document Information
//
// *Implicit depends:* DOM
//
// Store and retrieve global information about doctypes and context.

// ## Imported Modules

var S = require('../sender.js');
var ajax = require('ajax');
var ui = require('documents/ui-shared');

// ## Exported Function Names

var checkState;
var clearSession;
var doctypeId;
var identifier;
var info;
var loadDoctype;
var makeFieldsetLookup;
var makeLabels;
var project;
var setsKey;
var setVersion;
var worksheetName;

// ## Internal Function Names

var fieldsToFieldsetKey;
var getCurrentVersion;
var getVersion;
var infoKey;
var isAllDataStored;
var isCurrentVersionStored;
var isFieldsToFieldsetStored;
var isInfoStored;
var isLabelsStored;
var labelsKey;
var storeDoctype;
var versionKey;

// ## Internal Functions

// Key used in retrieving cached information from session storage.
fieldsToFieldsetKey = function () {
  'use strict';

  return identifier() + '_fieldsToFieldset';
};

// Get the most recent doctype version, which is placed in a `data`
// attribute that is updated on page reloads.
getCurrentVersion = function () {
  'use strict';

  return ui.allDocContainer().dataset.documentVersion;
};

// Get the stored doctype version.
getVersion = function () {
  'use strict';

  return sessionStorage.getItem(versionKey());
};

// Key used in retrieving cached information from session storage.
infoKey = function () {
  'use strict';

  return identifier() + '_info';
};

// True if the data that should be stored is stored.
isAllDataStored = function () {
  'use strict';

  return isCurrentVersionStored() && isInfoStored() && isLabelsStored() && isFieldsToFieldsetStored();
};

// Check if the stored doctype version matches the version found in the
// `data` attribute.
isCurrentVersionStored = function () {
  'use strict';

  return (getVersion() && getVersion() === getCurrentVersion());
};

// Is the field to fieldsets index stored?
isFieldsToFieldsetStored = function () {
  'use strict';

  return sessionStorage.getItem(fieldsToFieldsetKey()) !== null;
};

// Is the doctype information stored?
isInfoStored = function () {
  'use strict';

  return sessionStorage.getItem(infoKey()) !== null;
};

// Is the field plus fieldset to labels index stored?
isLabelsStored = function () {
  'use strict';

  return sessionStorage.getItem(labelsKey()) !== null;
};

// Key used in retrieving cached information from session storage.
labelsKey = function () {
  'use strict';

  return identifier() + '_labels';
};

// Store the doctype info in the session store.
storeDoctype = function (doctype) {
  'use strict';

  sessionStorage.setItem(infoKey(), doctype);

  return S.sender('doctype-info-ready');
};

// Key used in retrieving cached information from session storage.
versionKey = function () {
  'use strict';

  return identifier() + '_version';
};

// ## Exported Functions

// Check the session state to ensure it is up to date and fully
// loaded.
checkState = function () {
  'use strict';

  var retval;

  if (isAllDataStored()) {
    retval = S.sender('doctype-cached-info-ready');
  } else {
    retval = S.sender('bad-session-state');
  }

  return retval;
};

// Clear the session storage
clearSession = function () {
  'use strict';

  sessionStorage.clear();
  S.sender('session-cleared');

  return true;
};

// Get the doctype name
doctypeId = function () {
  'use strict';

  return ui.allDocContainer().dataset.documentDoctype;
};

// Identifier is a combination of the project and doctype name.
identifier = function () {
  'use strict';

  return project() + '_' + doctypeId();
};

// Get information about doctype.
info = function () {
  'use strict';

  var documentInfo = JSON.parse(sessionStorage.getItem(infoKey()));

  return documentInfo;
};

// Load the doctype document stored on the server.
loadDoctype = function () {
  'use strict';

  ajax.get('./', function (req) {
    storeDoctype(JSON.stringify(req.response));
  });

  return true;
};

// Process the field and fieldset info to create a field id to fieldset
// id index.
makeFieldsetLookup = function () {
  'use strict';

  var lookup = {};

  info().fieldsets.forEach(function (fieldset) {
    fieldset.fields.forEach(function (field) {
      lookup[field._id] = fieldset._id;
    });
  });

  sessionStorage.setItem(fieldsToFieldsetKey(), JSON.stringify(lookup));

  return S.sender('fieldset-lookup-ready');
};

// Process the field and fieldset info to create a field id to field
// label index.
makeLabels = function () {
  'use strict';

  var labels = {};

  info().fieldsets.forEach(function (fieldset) {
    fieldset.fields.forEach(function (field) {
      labels[field._id] = [fieldset.label, field.label];
    });
  });

  sessionStorage.setItem(labelsKey(), JSON.stringify(labels));

  return S.sender('doctype-cached-info-ready');
};

// Get the project id
project = function () {
  'use strict';

  return ui.container().dataset.projectId;
};

// Get the key that stores sets.
setsKey = function () {
  'use strict';

  return identifier() + '_sets';
};

// Reset the doctype version
setVersion = function () {
  'use strict';

  sessionStorage.setItem(versionKey(), getCurrentVersion());
  S.sender('version-set');

  return true;
};

// Name for the worksheet template.
var worksheetName = function () {
  'use strict';

  return identifier() + '_worksheet-template';
};

exports.checkState = checkState;
exports.clearSession = clearSession;
exports.doctypeId = doctypeId;
exports.identifier = identifier;
exports.info = info;
exports.loadDoctype = loadDoctype;
exports.makeFieldsetLookup = makeFieldsetLookup;
exports.makeLabels = makeLabels;
exports.project = project;
exports.setsKey = setsKey;
exports.setVersion = setVersion;
exports.worksheetName = worksheetName;

},{"../sender.js":97,"ajax":105,"documents/ui-shared":124}],71:[function(require,module,exports){
// # The search user interface
//
// *Implicit depends:* DOM, JQuery
//
// Handles the search user interface.

// Variable Definitions

var templates = require('templates');
var utils = require('utils');
var sets = require('sets');
var setsui = require('documents/setsui');
var ui = require('documents/ui-shared');
var info = require('documents/information');
var ajax = require('ajax');
var multipleFields;
var loadSearchVals;

// Internal functions

// User interface element
var searchIndex = function () {
  'use strict';

  return document.getElementById('document-search-index');
};

// User interface element
var searchIndexLabel = function () {
  'use strict';

  return document.getElementById('search-index-label');
};

// User interface element
var searchTerm = function () {
  'use strict';

  return document.getElementById('document-search-term');
};

// User interface element
var searchFields = function () {
  'use strict';

  return document.getElementById('document-search-field');
};

// User interface element
var searchFieldsLabel = function () {
  'use strict';

  return document.getElementById('document-search-label');
};

// User interface element
var searchExclude = function () {
  'use strict';

  return document.getElementById('document-search-exclude');
};

// User interface element
var searchInvert = function () {
  'use strict';

  return document.getElementById('document-search-invert');
};

// User interface element
var searchAll = function () {
  'use strict';

  return document.getElementById('search-all-fields-switch');
};

// User interface element
var searchListing = function () {
  'use strict';

  return document.getElementById('search-listing');
};

// User interface element
var getIdentifier = function () {
  'use strict';

  return info.identifier();
};

// All the form elements.
var formElems = [searchIndex, searchIndexLabel, searchFields, searchFieldsLabel, searchExclude, searchInvert, searchAll];

// If searching a user created index, the value of the hidden input
// where the index id specified.
var indexVal = function () {
  'use strict';

  var val = ui.indexIndexInput().value;

  if (val.length === 0) {
    return null;
  } else {
    return val;
  }
};

// Used for values that must either be true or null.
var maybeTrue = function (bool) {
  'use strict';

  if (bool) {
    return true;
  } else {
    return null;
  }
};

// Clear all search information that is stored in local storage.
var clearStore = function () {
  'use strict';

  var ident = getIdentifier();
  localStorage.setItem(ident + '_searchIndex', null);
  localStorage.setItem(ident + '_searchIndexLabel', null);
  localStorage.setItem(ident + '_searchFields', null);
  localStorage.setItem(ident + '_searchExclude', null);
  localStorage.setItem(ident + '_searchInvert', null);
};

// Clear the search form.
var clearVals = function () {
  'use strict';

  formElems.forEach(function (x) {
    var elem = x();
    switch (elem.getAttribute('type')) {
    case 'hidden':
      elem.value = '';
      break;
    case 'checkbox':
      elem.checked = false;
      break;
    }
  });
};

// Hide all the form elements.
var hideElems = function () {
  'use strict';

  formElems.forEach(function (x) {
    var elem = x();
    switch (elem.getAttribute('type')) {
    case 'hidden':
      break;
    case 'checkbox':
      ui.hide(elem.parentElement);
      break;
    default:
      ui.hide(elem);
    }
  });
};

// Get the field labels from session storage.
var fieldLabels = function () {
  'use strict';

  var ident = getIdentifier();
  var fieldlabels = JSON.parse(sessionStorage.getItem(ident + '_labels'));
  return fieldlabels;
};

// Render the search field item template using given values.
var searchFieldItem = function (field, fieldLabel) {
  'use strict';

  return templates['search-field-item']({
    fieldLabel: fieldLabel,
    field: field
  });
};

// Set the fields to search.
var setFields = function (fields) {
  'use strict';

  var fLabels = fieldLabels();
  var jFields = JSON.stringify(fields);
  var sfls = searchFieldsLabel();
  var ident = getIdentifier();

  searchFields().value = jFields;
  localStorage.setItem(ident + '_searchFields', jFields);

  var linkLabels = fields.map(function (x) {
    return searchFieldItem(x, fLabels[x].join(': '));
  });

  sfls.innerHTML = linkLabels.join(' ');

  return true;
};

// Exported functions

// Put the form in a state where all fields will be searched.
var allFields = function () {
  'use strict';

  clearStore();
  hideElems();
  clearVals();

  return true;
};

// Put the form in a state where one field will be searched.
var singleField = function (fields) {
  'use strict';

  multipleFields(fields);
  ui.show(searchInvert().parentElement);

  return true;
};

// Put the form in a state where one field will be used to perform an
// inverse search.
var singleFieldInverse = function (fields) {
  'use strict';

  var ident = getIdentifier();
  singleField(fields);
  searchInvert().checked = true;
  localStorage.setItem(ident + '_searchInvert', true);

  return true;
};

// Put the form in a state where multiple fields will be searched.
multipleFields = function (fields) {
  'use strict';

  allFields();
  setFields(fields);
  [searchAll(), searchFieldsLabel(), searchExclude().parentElement].forEach(function (x) {
    ui.show(x);
  });

  return true;
};

// Put the form in a state where fields will be excluded from search.
var excludedFields = function (fields) {
  'use strict';

  var ident = getIdentifier();

  if (fields.length > 1) {
    multipleFields(fields);
  } else {
    singleField(fields);
  }

  searchExclude().checked = true;
  localStorage.setItem(ident + '_searchExclude', true);

  return true;
};

// Put the form in a state where a user created index will be searched.
var indexOnly = function (index, indexLabel) {
  'use strict';

  var ident = getIdentifier();

  allFields();
  localStorage.setItem(ident + '_searchIndex', index);
  localStorage.setItem(ident + '_searchIndexLabel', indexLabel);
  searchIndex().value = index;
  searchIndexLabel().innerHTML = indexLabel;

  [searchAll(), searchIndex(), searchIndexLabel(), searchInvert().parentElement].forEach(function (x) {
    ui.show(x);
  });

  return true;
};

// Put the form in a state where a user created index will be used to
// perform an inverse search.
var indexInverse = function (index, indexLabel) {
  'use strict';

  var ident = getIdentifier();

  indexOnly(index, indexLabel);
  searchInvert().checked = true;
  localStorage.setItem(ident + '_searchInvert', true);

  return true;
};

// Perform the search.
var getSearch = function () {
  'use strict';

  var query = searchTerm().value;
  var url = 'documents/search?q=' + window.encodeURIComponent(query);
  var field = searchFields().value;
  var exclude = searchExclude().checked;
  var invert = searchInvert().checked;
  var index = searchIndex().value;
  var fieldlabels = fieldLabels();

  if (index) {
    url = url + '&index=' + index;
  } else {
    if (field) {
      url = url + '&field=' + field;
    }
    if (exclude) {
      url = url + '&exclude=true';
    }
  }
  if (invert) {
    url = url + '&invert=true';
  }

  ui.hide(searchListing());

  ajax.get(url, function (req) {
    var results = req.response;
    var html;

    results.are_results = results.rows && results.rows.length > 0;

    if (results.index_listing) {
      results.rows = results.rows.map(function (row) {
        row.key = row.key.map(function (x) {
          return x[1];
        }).join(',');

        return row;
      });
    }

    html = templates['document-search'](results);
    searchListing().innerHTML = html;

    Array.prototype.forEach.call(document.getElementsByClassName('search-result-field-id'), function (item) {
      var label = fieldlabels[item.dataset.fieldField].join(': ');
      var target = item.children[0];

      target.innerHTML = label;
      target.dataset.searchLabel = label;
    });

    if (!invert) {
      Array.prototype.forEach.call(document.querySelectorAll('.search-results th'), function (item) {
        var itemText = item.children[0].innerHTML.replace(/(^\s|\s$)/g, '');
        var re = new RegExp('(' + query + ')', 'g');
        var newText = itemText.replace(re, '<span class="highlight">$1</span>');

        item.children[0].innerHTML = newText;
      });
    }

    ui.show(searchListing());
  });

  return true;
};

// Remove a field from those that will be searched (or excluded in an
// exclusive search.)
var removeField = function (t) {
  'use strict';

  var ident = getIdentifier();
  var searchFields = localStorage.getItem(ident + '_searchFields');
  var newSearchFields;
  var fields = JSON.parse(searchFields);
  var newFields;
  var id = t.dataset.fieldField;

  if (fields !== null) {
    newFields = fields.filter(function (x) {
      return x !== id;
    });
    newSearchFields = JSON.stringify(newFields);
    localStorage.setItem(ident + '_searchFields', (newFields.length === 0) ? null : newSearchFields);
    localStorage.setItem(ident + '_searchIndex', null);
    loadSearchVals();
  }

  return true;
};

// Add a field to those that will be searched (or excluded in an
// exclusive search.)
var addField = function (t) {
  'use strict';

  var ident = getIdentifier();
  var searchFields = localStorage.getItem(ident + '_searchFields');
  var newSearchFields;
  var fields = JSON.parse(searchFields);
  var newFields;
  var id = t.dataset.fieldField;

  if (fields === null) {
    fields = [];
  }

  newFields = sets.union(fields, id);
  newSearchFields = JSON.stringify(newFields);
  localStorage.setItem(ident + '_searchFields', (newFields.length === 0) ? null : newSearchFields);
  localStorage.setItem(ident + '_searchIndex', null);
  loadSearchVals();

  return true;
};

// Add a user created index to be searched.
var addIndex = function () {
  'use strict';

  var val = indexVal();
  var ident = getIdentifier();

  if (val) {
    localStorage.setItem(ident + '_searchFields', null);
    localStorage.setItem(ident + '_searchIndex', val);
    localStorage.setItem(ident + '_searchIndexLabel', $('option[value=' + val + ']').html());
    loadSearchVals();
  }

  return true;
};

// Toggle the inverse search setting.
var toggleInversion = function () {
  'use strict';

  var ident = getIdentifier();

  localStorage.setItem(ident + '_searchInvert', maybeTrue(searchInvert().checked));
  localStorage.setItem(ident + '_searchExclude', null);
  loadSearchVals();

  return true;
};

// Toggle the exclusive search setting.
var toggleExclusion = function () {
  'use strict';

  var ident = getIdentifier();

  localStorage.setItem(ident + '_searchExclude', maybeTrue(searchExclude().checked));
  localStorage.getItem(ident + '_searchInvert', null);
  loadSearchVals();

  return true;
};

// The functions that alter the search form above store the values in
// local storage. This interprets those values and puts the search form
// in a consistent state.
loadSearchVals = function () {
  'use strict';

  var ident = getIdentifier();
  var exclude = localStorage.getItem(ident + '_searchExclude');
  var invert = localStorage.getItem(ident + '_searchInvert');
  var index = localStorage.getItem(ident + '_searchIndex');
  var fieldids = localStorage.getItem(ident + '_searchFields');
  var fields;
  var indexLabel;
  var params = [exclude, invert, index, fieldids].map(function (x) {
    return (x === 'null' || x === 'false' || x === 'true') ? JSON.parse(x) : x;
  });
  var allNull = params.every(function (x) {
    return x === null;
  });

  try {
    if (allNull) {
      allFields();
    } else if (params[0] === true) {
      fields = JSON.parse(fieldids);
      excludedFields(fields);
    } else if (params[1] === null && params[3] !== null) {
      fields = JSON.parse(fieldids);
      if (fields.length > 1) {
        multipleFields(fields);
      } else {
        singleField(fields);
      }
    } else if (params[3] !== null) {
      fields = JSON.parse(fieldids);
      if (fields.length > 1) {
        multipleFields(fields);
      } else {
        singleFieldInverse(fields);
      }
    } else if (params[1] === null) {
      indexLabel = localStorage.getItem(ident + '_searchIndexLabel');
      indexOnly(index, indexLabel);
    } else if (params[1] === true) {
      indexLabel = localStorage.getItem(ident + '_searchIndexLabel');
      indexInverse(index, indexLabel);
    }
  } catch (e) {
    allFields();
  }

  return true;
};

// Toggle selection of result to save to set.
var toggleSelection = function (target) {
  'use strict';

  if (target.checked) {
    target.nextSibling.nextSibling.classList.add('selected-for-save');
  } else {
    target.nextSibling.nextSibling.classList.remove('selected-for-save');
  }

  return true;
};

exports.addField = addField;
exports.addIndex = addIndex;
exports.allFields = allFields;
exports.excludedFields = excludedFields;
exports.getSearch = getSearch;
exports.indexInverse = indexInverse;
exports.indexOnly = indexOnly;
exports.loadSearchVals = loadSearchVals;
exports.multipleFields = multipleFields;
exports.removeField = removeField;
exports.singleField = singleField;
exports.singleFieldInverse = singleFieldInverse;
exports.toggleExclusion = toggleExclusion;
exports.toggleInversion = toggleInversion;
exports.toggleSelection = toggleSelection;

},{"ajax":105,"documents/information":121,"documents/setsui":123,"documents/ui-shared":124,"sets":148,"templates":52,"utils":150}],72:[function(require,module,exports){
// # The sets user interface
//
// *Implicit depends:* DOM, JQuery
//
// Handles the sets user interface.

// Variable Definitions

var templates = require('templates');
var S = require('../sender.js');
var flash = require('flash');
var sets = require('sets');
var utils = require('utils');
var info = require('documents/information');
var removeSet;
var setSets;
var selectedElementsToArray;
var selectedSaveResultsToArray;
var render;
var getSets;
var getSet;

// Internal functions

// User interface element
var setA = function () {
  'use strict';

  return $('#document-set-a-input');
};

// User interface element
var setB = function () {
  'use strict';

  return $('#document-set-b-input');
};

// User interface element
var worksheetsSet = function () {
  'use strict';

  return $('#document-worksheets-set-input');
};

// User interface element
var op = function () {
  'use strict';

  return $('#document-set-operation-input');
};

// User interface element
var setListing = function () {
  'use strict';

  return $('#set-listing');
};

// User interface element
var sessionKey = function () {
  'use strict';

  return info.setsKey();
};

// Custom member function to use with [sets.js](./sets.html).
var member = function (arr, x) {
  'use strict';

  return arr.some(function (y) {
    return x[0] === y[0] && x[1] === y[1];
  });
};

// Ensure that the set is correct.
var processSet = function (set) {
  'use strict';

  var name = set [0];
  var arr = sets.unique(set [1], member);
  var procSet = [name, arr];
  return procSet;
};

// Perform the union of the sets specified by the user interface.
var union = function (setNameA, setNameB) {
  'use strict';

  var setElemsA = getSet(setNameA)[1];
  var setElemsB = getSet(setNameB)[1];
  var newSet = sets.union(setElemsA, setElemsB, member);
  render(newSet);
  return true;
};

// Perform the intersection of the sets specified by the user interface.
var intersection = function (setNameA, setNameB) {
  'use strict';

  var setElemsA = getSet(setNameA)[1];
  var setElemsB = getSet(setNameB)[1];
  var newSet = sets.intersection(setElemsA, setElemsB, member);
  render(newSet);
  return true;
};

// Perform the relative complement of the sets specified by the user
// interface.
var relativeComplement = function (setName1, setName2) {
  'use strict';

  var setElems1 = getSet(setName1)[1];
  var setElems2 = getSet(setName2)[1];
  var newSet = sets.relativeComplement(setElems1, setElems2, member);
  render(newSet);

  return true;
};

// Perform the symmetric difference of the sets specified by the user
// interface.
var symmetricDifference = function (setNameA, setNameB) {
  'use strict';

  var setElemsA = getSet(setNameA)[1];
  var setElemsB = getSet(setNameB)[1];
  var newSet = sets.symmetricDifference(setElemsA, setElemsB, member);
  render(newSet);

  return true;
};

// Get the sets saved in session storage
getSets = function () {
  'use strict';

  var curr = window.sessionStorage.getItem(sessionKey());
  var retval = [];

  if (curr !== null) {
    retval = JSON.parse(curr);
  }

  return retval;
};

// View a set.
var view = function (setName) {
  'use strict';

  var elems = getSet(setName)[1];
  render(elems);

  return true;
};

// Remove a set.
var remove = function (setName) {
  'use strict';

  removeSet(setName);
  render([]);
  S.sender('sets-changed');

  return true;
};

// Perform set removal.
removeSet = function (setName) {
  'use strict';

  var nnew;
  var curr = getSets();
  nnew = curr.filter(function (x) {
    return x[0] !== setName;
  });
  setSets(nnew);

  return true;
};

// Retrieve the set names.
var getSetNames = function () {
  'use strict';

  var curr = getSets();
  return curr.map(function (x) {
    return x[0];
  });
};

// Save sets to session storage.
setSets = function (nnew) {
  'use strict';

  var procSets;
  if (Array.isArray(nnew)) {
    procSets = nnew.map(function (x) {
      return processSet(x);
    });
    window.sessionStorage.setItem(sessionKey(), JSON.stringify(procSets));
  } else {
    window.sessionStorage.settem(sessionKey(), '[]');
  }

  return true;
};

// Save a set to session storage.
var setSet = function (nnew) {
  'use strict';

  if (Array.isArray(nnew) && nnew.length === 2) {
    var curr = getSets();
    var newName = nnew[0];
    var filtered = curr.filter(function (x) {
      return x[0] !== newName;
    });
    setSets(filtered.concat([nnew]));
  }
  return true;
};

// Convert selected search results or a selected elements to an array.
var selectedToArray = function (target) {
  'use strict';

  var retval = [];

  switch (target) {
  case 'search':
    retval = selectedSaveResultsToArray();
    break;
  case 'sets':
    retval = selectedElementsToArray();
    break;
  }

  return retval;
};

// Convert selected elements to an array.
selectedElementsToArray = function () {
  'use strict';

  var retval;
  var selected = $('input.set-element-selection:checked');

  retval = $.map(selected, function (elem) {
    var anchor = $(elem).parent('td').next('td').find('a').first();
    var id = anchor.first().attr('href').replace(/^#/, '');
    var context = anchor.html().trim();
    return [
      [context, id]
    ];
  });
  return retval;
};

// Convert selected search results to an array.
selectedSaveResultsToArray = function () {
  'use strict';

  var retval;
  var selected = $('table.selected-for-save tr');

  retval = $.map(selected, function (elem) {
    var id = $(elem).find('th a').first().attr('href').replace(/^#/, '');
    var context = $(elem).find('td.search-result-context a').first().html().trim();
    return [
      [context, id]
    ];
  });

  return retval;
};

// Render the set for display.
render = function (setElems) {
  'use strict';

  var total = setElems.length;
  var elems = setElems.map(function (x) {
    return {
      id: x[1],
      context: x[0]
    };
  });
  var listing = templates['set-listing']({
    elements: elems,
    total: total
  });
  setListing().html(listing);
  return true;
};

// Exported functions

// Retrieve a set.
var getSet = function (setName) {
  'use strict';

  var retval;
  var curr = getSets();
  retval = curr.filter(function (x) {
    return x[0] === setName;
  })[0];
  return retval;
};

// Perform a set operation.
var performOp = function () {
  'use strict';

  switch (op().val()) {
  case 'view-a':
    view(setA().val());
    break;
  case 'view-b':
    view(setB().val());
    break;
  case 'remove-a':
    remove(setA().val());
    break;
  case 'remove-b':
    remove(setB().val());
    break;
  case 'union':
    union(setA().val(), setB().val());
    break;
  case 'intersection':
    intersection(setA().val(), setB().val());
    break;
  case 'symmetric-difference':
    symmetricDifference(setA().val(), setB().val());
    break;
  case 'relative-complement-b-in-a':
    relativeComplement(setA().val(), setB().val());
    break;
  case 'relative-complement-a-in-b':
    relativeComplement(setB().val(), setA().val());
    break;
  default:
    break;
  }
  return true;
};

// Update the selection of sets to choose from.
var updateSelection = function () {
  'use strict';

  var currNames = getSetNames();
  var newOptions = templates['set-options']({
    names: currNames
  });
  setA().html(newOptions);
  setB().html(newOptions);
  worksheetsSet().html(newOptions);

  return true;
};

// Save select items as a set.
var saveSelected = function () {
  'use strict';

  var dialog = $('#new-set-dialog');
  var name = $('#new-set-input').val();
  var target = $('#new-set-target-input').val();
  var selected;
  var newSet;

  if (!utils.isBlank(name)) {
    dialog.hide();
    selected = selectedToArray(target);
    newSet = [name, selected];
    setSet(newSet);
    $('#new-set-input').val('');
    S.sender('sets-changed');
    flash.highlight('Success:', 'Set "' + name + '" saved.');
  } else {
    flash.error('Input invalid:', 'You must supply a valid name.');
  }

  return true;
};

// Toggle the selection of all elements.
var toggleSelectAll = function (target) {
  'use strict';

  if ($(target).is(':checked')) {
    $('input.set-element-selection').prop('checked', true);
  } else {
    $('input.set-element-selection').prop('checked', false);
  }
  return true;
};

exports.getSet = getSet;
exports.performOp = performOp;
exports.updateSelection = updateSelection;
exports.saveSelected = saveSelected;
exports.toggleSelectAll = toggleSelectAll;

},{"../sender.js":97,"documents/information":121,"flash":128,"sets":148,"templates":52,"utils":150}],73:[function(require,module,exports){
// # UI Shared
//
// *Implicit depends:* DOM
//
// UI elements and helper functions.

var form = require('form');

// ## Exported functions

// User interface element
var allDocContainer = function () {
  'use strict';

  return document.getElementById('all-document-container');
};

// User interface element
var container = function () {
  'use strict';

  return document.getElementById('container');
};

// User interface element
var createButton = function () {
  'use strict';

  return document.getElementById('create-document-button');
};

// User interface element
var deleteButton = function () {
  'use strict';

  return document.getElementById('document-delete-button');
};

// User interface element
var dv = function () {
  'use strict';

  return document.getElementById('document-view');
};

// User interface element
var dvt = function () {
  'use strict';

  return document.getElementById('document-view-tree');
};

// User interface element
var editButton = function () {
  'use strict';

  return document.getElementById('document-edit-button');
};

// User interface element
var editForm = function () {
  'use strict';

  return document.getElementById('edit-document-form');
};

// User interface element
var firstIndex = function () {
  'use strict';

  return document.getElementById('first-index-element');
};

// User interface element
var indexIndexInput = function () {
  'use strict';

  return document.getElementById('index-index-input');
};

// User interface element
var restoreButton = function () {
  'use strict';

  return document.getElementById('document-restore-button');
};

// User interface element
var saveButton = function () {
  'use strict';

  return document.getElementById('save-document-button');
};

var sid = function () {
  'use strict';

  var fi;

  return fi ? fi.dataset.firstId : undefined;
};

var skey = function () {
  'use strict';

  var fi;

  return fi ? fi.dataset.firstKey : undefined;
};

// User interface element
var viewInfo = function () {
  'use strict';

  return document.getElementById('document-view-info');
};

exports.allDocContainer = allDocContainer;
exports.container = container;
exports.createButton = createButton;
exports.deleteButton = deleteButton;
exports.dv = dv;
exports.dvt = dvt;
exports.editButton = editButton;
exports.editForm = editForm;
exports.firstIndex = firstIndex;
exports.indexIndexInput = indexIndexInput;
exports.restoreButton = restoreButton;
exports.saveButton = saveButton;
exports.sid = sid;
exports.skey = skey;
exports.viewInfo = viewInfo;

exports.hide = form.hide;
exports.hideDisable = form.hideDisable;
exports.show = form.show;
exports.showEnable = form.showEnable;

},{"form":129}],74:[function(require,module,exports){
// # The view user interface
//
// *Implicit depends:* DOM
//
// View pane UI elements.
//
// *TODO* I may be exporting more than needed.

// Variable Definitions

var templates = require('templates');
var store = require('store').store;
var indexui = require('documents/indexui');
var flash = require('flash');
var ui = require('documents/ui-shared');
var editui = require('./editui.js');
var fieldsets = require('./fieldsets.js');
var ajax = require('ajax');

// Internal functions

// Make an object where fieldsets with deletions are identified.
var getDeletions = function (changes) {
  'use strict';

  return Object.keys(changes).reduce(function (acc, x) {
    // If it was changed and there is no new value, it was deleted.
    if (changes[x].newValue === undefined) {
      if (acc[changes[x].fieldset] === undefined) {
        acc[changes[x].fieldset] = {};
      }
      acc[changes[x].fieldset][x] = changes[x];
    }

    return acc;
  }, {});
};

// Process the document from the server.
var processIncoming = function (docJson, rev) {
  'use strict';

  var withDeletions = {};

  if (docJson.changes) {
    withDeletions = getDeletions(docJson.changes);
  }

  docJson.fieldsets.forEach(function (fset) {
    var fsetId = fset.id;

    if (withDeletions[fsetId] !== undefined) {
      fset.removal = true;
      fset.altered = true;
    }

    var fieldFunc = function (field) {
      var changes = {};
      var change;

      if (docJson.changes) {
        changes = docJson.changes;
      }
      change = changes[field.instance];

      field.json_value = JSON.stringify(field.value);

      if (change !== undefined) {
        field.changed = true;
        fset.altered = true;

        if (change.originalValue === undefined) {
          fset.addition = true;
          field.newfield = true;
        } else {
          field.originalValue = JSON.parse(change.originalValue);
        }
      }

      if (field.subcategory === 'textarea') {
        field.is_textarea = true;
      } else if (field.value && field.subcategory.match('multi')) {
        field.value = field.value.join(', ');
      }

      return true;
    };

    if (fset.multiple) {
      fset.multifields.forEach(function (mfs) {
        mfs.fields.forEach(function (field) {
          fieldFunc(field);
          return true;
        });
      });
    } else {
      fset.fields.forEach(function (field) {
        fieldFunc(field);
        return true;
      });
    }

    return true;
  });

  return true;
};

// Exported functions

// Format the 'update at' and 'created at' timestamps and localize them
// to the current time zone.
var formatTimestamps = function () {
  'use strict';

  Array.prototype.forEach.call(document.querySelectorAll('.timestamp'), function (item) {
    var newDate = (new Date(item.textContent)).toLocaleString();
    if (newDate !== 'Invalid Date') {
      item.textContent = newDate;
    }
  });

  return true;
};

// Get the document.
var get = function (id, rev, callback) {
  'use strict';

  var url = 'documents/' + id;
  var htmlTarget = ui.dv();
  var tmpl;

  if (rev) {
    url = url + '/' + rev;
    htmlTarget = ui.dvt();
    tmpl = function (docJson) {
      return templates['document-view-tree'](docJson);
    };
  } else {
    tmpl = function (docJson) {
      return templates['document-view'](docJson);
    };

  }

  ajax.get(url, function (req) {
    var documentHtml;
    var docJson = req.response;

    processIncoming(docJson, rev);
    documentHtml = tmpl(docJson);
    htmlTarget.innerHTML = documentHtml;
    window.location.hash = id;
    formatTimestamps();
    ui.dv().style.opacity = 1;
    if (callback) {
      callback();
    }

    if (rev) {
      ui.dvt().classList.add('oldrev');
    } else {
      if (store(ui.restoreButton()).d('deleted') === 'true') {
        ui.hideDisable(ui.editButton());
        ui.hideDisable(ui.deleteButton());
        ui.showEnable(ui.restoreButton());
      }
    }
  });

  return true;
};

// Restore the state of a document to that of an earlier revision.
var restore = function (id, rev) {
  'use strict';

  var url = './documents/' + id + '?rev=' + rev;
  var body;
  var title;
  var statusCallbacks = [];
  statusCallbacks[200] = function (req) {
    title = 'Success';
    body = 'Your document was restored.';

    get(id, null, function () {
      ui.dv().style.opacity = 1;
      indexui.get(ui.skey(), ui.sid());
    });
    flash.highlight(title, body);
  };
  var errorCallback = function (req) {
    body = JSON.parse(req.responseText);
    title = req.statusText;

    flash.error(title, body.message);
  };
  statusCallbacks[409] = errorCallback;
  statusCallbacks[404] = errorCallback;

  ajax.del(url, undefined, statusCallbacks);

  return true;
};

// Delete the document.
var del = function (id, rev) {
  'use strict';

  var url = './documents/' + id + '?rev=' + rev;
  var body;
  var title;
  var statusCallbacks = [];
  statusCallbacks[200] = function (req) {
    title = 'Success';
    body = 'Your document was deleted.';

    store(ui.restoreButton()).put('document-rev', req.response.rev);

    ui.hideDisable(ui.deleteButton());
    ui.hideDisable(ui.editButton());
    ui.showEnable(ui.restoreButton());
    ui.dv().style.opacity = 0.5;

    indexui.get(ui.skey(), ui.sid());
    flash.highlight(title, body);
  };
  var errorCallback = function (req) {
    body = JSON.parse(req.responseText);
    title = req.statusText;

    flash.error(title, body.message);
  };
  statusCallbacks[409] = errorCallback;
  statusCallbacks[404] = errorCallback;

  ajax.del(url, undefined, statusCallbacks);

  return true;
};

// Confirm an action.
var confirmIt = function (callback) {
  'use strict';

  if (window.confirm('Are you sure?')) {
    var s = store(ui.viewInfo());
    var id = s.d('document');
    var rev = s.d('rev');

    callback(id, rev);
  }

  return true;
};

// Move the document to the editor.
var edit = function () {
  'use strict';

  var sb = ui.saveButton();

  editui.clear();
  if (ui.dvt().classList.contains('oldrev')) {
    sb.classList.add('oldrev');
  } else {
    sb.classList.remove('oldrev');
  }
  fieldsets.fillFieldsets();

  return true;
};

// Ask for confirmation on deletion.
var confirmDelete = function () {
  'use strict';

  var s = store(ui.viewInfo());
  var id = s.d('document');
  var rev = s.d('rev');

  return confirmIt(function () {
    del(id, rev);
  });
};

// Ask for confirmation on restoration.
var confirmRestore = function () {
  'use strict';

  var s = store(ui.viewInfo());
  var id = s.d('document');
  var rev = s.d('rev');

  return confirmIt(function () {
    restore(id, rev);
  });
};

// Expand and collapse elements of the view tree.
var collapseToggle = function (target) {
  'use strict';

  target.parentElement.classList.toggle('collapsed');

  return true;
};

// Get a previous revision.
var fetchRevision = function (target) {
  'use strict';

  var s = store(target);
  var id = s.d('document');
  var oldrev = s.d('oldrev');

  Array.prototype.forEach.call(document.getElementsByClassName('revision-link'), function (item) {
    item.classList.remove('selected-revision');
  });

  target.classList.add('selected-revision');

  get(id, oldrev);

  return true;
};

exports.formatTimestamps = formatTimestamps;
exports.get = get;
exports.restore = restore;
exports.del = del;
exports.confirmIt = confirmIt;
exports.edit = edit;
exports.confirmDelete = confirmDelete;
exports.confirmRestore = confirmRestore;
exports.collapseToggle = collapseToggle;
exports.fetchRevision = fetchRevision;

},{"./editui.js":67,"./fieldsets.js":68,"ajax":105,"documents/indexui":120,"documents/ui-shared":124,"flash":128,"store":149,"templates":52}],75:[function(require,module,exports){
// # The worksheet user interface
//
// *Implicit depends:* DOM, JQuery, globals
// ([application.js](./application.html))
//
// Worksheet pane UI elements.

// Variable Definitions

var Hogan = require('hogan.js');
var templates = require('templates');
var setsui = require('documents/setsui');
var info = require('documents/information');
var ajax = require('ajax');
var flash = require('flash');

// Internal functions

// User interface element
var worksheetsSet = function () {
  'use strict';

  return $('#document-worksheets-set-input');
};

// User interface element
var worksheetsArea = function () {
  'use strict';

  return $('#worksheet-area');
};

// Exported functions

// Select all the visible rows.
var selectAllRows = function (select) {
  'use strict';

  if (select) {
    $('#worksheet-table tbody tr').addClass('selected-row');
    $('#worksheet-table tbody tr input').prop('checked', true);
  } else {
    $('#worksheet-table tbody tr').removeClass('selected-row');
    $('#worksheet-table tbody tr input:checked').prop('checked', false);
  }

  return true;
};

// Set the proper class for a selected row and unset the 'select all'
var rowSelection = function (row, select) {
  'use strict';

  if (select) {
    $('#' + row).addClass('selected-row');
    $('#select-all-worksheet-rows').prop('checked', false);
  } else {
    $('#' + row).removeClass('selected-row');
    $('#select-all-worksheet-rows').prop('checked', false);
  }

  return true;
};

// Select a column.
var columnSelection = function (column, select) {
  'use strict';

  if (select) {
    $('.field-column.' + column).addClass('selected-column');
  } else {
    $('.field-column.' + column).removeClass('selected-column');
  }

  return true;
};

// Show vertical headers for fields and fieldsets.
var showHandles = function () {
  'use strict';

  $('#worksheet-table .handle-column.fieldset').show();

  return true;
};

// Hide vertical headers for fields and fieldsets.
var hideHandles = function () {
  'use strict';

  $('#worksheet-table .handle-column.fieldset').hide();

  return true;
};

// Show the fieldset handle.
var showFieldset = function (fsid) {
  'use strict';

  $('#worksheet-table .handle-column.field.' + fsid).show();

  return true;
};

// Hide the fieldset handle.
var hideFieldset = function (fsid) {
  'use strict';

  $('#worksheet-table .handle-column.field.' + fsid).hide();

  return true;
};

// Show a field.
var showField = function (fid) {
  'use strict';

  $('.field-column.' + fid).show();

  return true;
};

// Hide a field.
var hideField = function (fid) {
  'use strict';

  $('.field-column.' + fid).hide();

  return true;
};

// There are two layers of templating information in the
// template. Activate the second layer.
var buildTemplate = function () {
  'use strict';

  var doctypeInfo = info.info();
  var metaTemp = '{{=<% %>=}}\n' + templates['worksheet'](doctypeInfo);
  globals[info.worksheetName()] = Hogan.compile(metaTemp);

  return true;
};

// Render the worksheet.
var fillWorksheet = function () {
  'use strict';

  var setName = worksheetsSet().val();
  var url = 'worksheets';
  var complete = function (req) {
    var ws = globals[info.worksheetName()].render(req.response);
    worksheetsArea().html(ws);
  };

  if (!setName.isBlank()) {
    var thisSet = setsui.getSet(setName)[1];

    if (thisSet.length <= 250) {
      var setIds = thisSet.map(function (x) {
        return x[1];
      });

      ajax.post(url, setIds, complete);
    } else {
      flash.error('Could not load worksheet', 'the current set size is limited to 250 items.');
    }
  }

  return true;
};

exports.selectAllRows = selectAllRows;
exports.rowSelection = rowSelection;
exports.columnSelection = columnSelection;
exports.showHandles = showHandles;
exports.hideHandles = hideHandles;
exports.showFieldset = showFieldset;
exports.hideFieldset = hideFieldset;
exports.showField = showField;
exports.hideField = hideField;
exports.buildTemplate = buildTemplate;
exports.fillWorksheet = fillWorksheet;

},{"ajax":105,"documents/information":121,"documents/setsui":123,"flash":128,"hogan.js":24,"templates":52}],76:[function(require,module,exports){
// # The file manager
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Interface for working with CouchDB attachments within documents that
// exist only for the pupose of holding the attachment. A mock-file
// system path is given to these saved documents and may be used to
// retrieve them instead of the ID.

// Variable Definitions

var ajax = require('ajax');
var flash = require('flash');
var refreshListings;

// Internal functions

// Get information subdirectories within a path. As an example
// '/home/chuck/'.
var getDirListing = function (path) {
  'use strict';

  if (path === undefined) {
    path = '';
  }

  ajax.legacyHTMLGet('file_manager/list_dirs/' + path, function (req) {
    $('#file-paths').html(req.response);
  });
};

// Get the document information for documents with a certain path.
var getFileListing = function (path) {
  'use strict';

  if (path === undefined) {
    path = '';
  }

  ajax.legacyHTMLGet('file_manager/list_files/' + path, function (req) {
    $('#file-listing').html(req.response);
  });
};

// Open a dialog for editing a file path.
var pathEditDialog = function (obj, path) {
  'use strict';

  var pathInput = $('#file-path-input');

  if (obj.path) {
    pathInput.val(obj.path.join('/'));
  } else {
    pathInput.val('');
  }

  var dialog = $('#edit-path-dialog').dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      'Move': function () {
        var url = 'file_manager/' + obj._id + '?rev=' + obj._rev;
        var complete = function () {
          refreshListings(path);
          flash.highlight('Success', 'File Moved');
        };

        obj.path = pathInput.val().replace(/^\s*|\s*$/g, '').replace(/\/+/g, '/').replace(/^\/|\/$/g, '').split('/');
        ajax.put(url, obj, complete);
        $(this).dialog('close');
      },
      'Cancel': function () {
        $(this).dialog('close');
      }
    }
  });

  return dialog;
};

// Exported functions

// Initialize the sub-application.
var init = function () {
  'use strict';

  refreshListings();
  $('#file-upload-target').load(function () {
    var encoded = $('#file-upload-target').contents().find('body pre').html();
    var obj = function () {
      if (encoded && encoded.length > 0) {
        return JSON.parse(encoded);
      } else {
        return {
          message: false
        };
      }
    };

    if (obj() && obj().message && obj().status !== 'success') {
      flash.error('Error', obj().message);
      refreshListings();
    } else if (obj().message) {
      flash.highlight('Success', obj().message);
      refreshListings();
    }
  });
};

// Handle the mouse click action that initiates going to a directory.
var goDir = function (target) {
  'use strict';

  var newpath = $(target).attr('data-path');
  window.sessionStorage.fmPath = newpath;
  refreshListings(newpath);

  return true;
};

// Return to the root directory.
var rootDir = function () {
  'use strict';

  var path = window.sessionStorage.fmPath = '';
  refreshListings();

  return true;
};

// Move up a directory.
var upDir = function () {
  'use strict';

  var path = window.sessionStorage.fmPath;
  var newpath = path.split('/');
  newpath.pop();
  newpath = newpath.join('/');
  window.sessionStorage.fmPath = newpath;

  refreshListings(newpath);

  return true;
};

// Handle the mouse click action that initiates editing a file by opening
// a dialog to edit its path.
var editFile = function (target) {
  'use strict';

  var path = window.sessionStorage.fmPath;
  var fileId = target.attr('data-file-id');
  var url = 'file_manager/' + fileId;

  ajax.get(url, function (req) {
    pathEditDialog(req.response, path).dialog('open');
  });

  return true;
};

// Handle the mouse click action that initiates deleting a file.
var deleteFile = function (target) {
  'use strict';

  var path = window.sessionStorage.fmPath;
  var fileId = target.attr('data-file-id');
  var fileRev = target.attr('data-file-rev');
  var url = 'file_manager/' + fileId + '?rev=' + fileRev;
  var complete = function () {
    refreshListings(path);
    flash.highlight('Success', 'File Deleted');
  };

  ajax.del(url, complete);

  return true;
};

// Refresh the file listing using the given path.
refreshListings = function (path) {
  'use strict';

  getDirListing(path);
  getFileListing(path);
};

exports.init = init;
exports.goDir = goDir;
exports.rootDir = rootDir;
exports.upDir = upDir;
exports.editFile = editFile;
exports.deleteFile = deleteFile;
exports.refreshListings = refreshListings;

},{"ajax":105,"flash":128}],77:[function(require,module,exports){
// # Brief Notification Messages
//
// *Implicit depends:* DOM, JQuery
//
// Helpers to display notifications.

// ## Internal Functions

// Helper function that handles the displaying and fading of the flashed
// notification.
var f = function (flasher, title, body) {
  'use strict';

  var fadeout = function () {
    flasher.fadeOut();
  };
  flasher.find('.notification-summary').text(title + ': ');
  flasher.find('.notification-message').text(body);
  var timeout = window.setTimeout(fadeout, 7000);
  flasher.fadeIn();
  flasher.find('.close').click(function () {
    window.clearTimeout(timeout);
    flasher.hide();
  });
};

// # Exported Functions

// Display an error.
var error = function (title, body) {
  'use strict';

  f($('#notifications-main .ui-state-error'), title, body);

  return true;
};

// Display a notification.
var highlight = function (title, body) {
  'use strict';

  f($('#notifications-main .ui-state-highlight'), title, body);

  return true;
};

exports.error = error;
exports.highlight = highlight;

},{}],78:[function(require,module,exports){
// # HTML Form Helpers
//
// *Implicit depends:* DOM, JQuery, JQueryUI
//
// Some form helpers.
// TODO: find non-JQueryUI implementations. Only the date picker needs
// JQuery or JQueryUI.

// ## Variable Definitions

var ajax = require('ajax');
var clear;

// ## Exported Functions

// Hide an element
var hide = function (elem) {
  'use strict';

  elem.classList.add('hidden');

  return document;
};

// Hide the button.
var hideDisable = function (elem) {
  'use strict';

  hide(elem);
  elem.setAttribute('disabled', 'disabled');

  return true;
};

// Display the element.
var show = function (elem) {
  'use strict';

  elem.classList.remove('hidden');

  return document;
};

// Display the button.
var showEnable = function (elem) {
  'use strict';

  show(elem);
  elem.removeAttribute('disabled');

  return true;
};

// Generic element toggler.
var toggle = function (target) {
  'use strict';

  var toggleElem = document.getElementById(target.dataset.target);

  toggleElem.classList.toggle('hidden');

  return target;
};

// Generic dialog canceling code
var cancelDialog = function (target) {
  'use strict';

  var toggleElem = document.getElementById(target.dataset.target);

  toggleElem.classList.add('hidden');
  clear(undefined, toggleElem.querySelector('form'));

  return target;
};

// Generic dialog form clearing code
clear = function (inputFields, form) {
  'use strict';

  if (inputFields === undefined) {
    inputFields = form.querySelectorAll('input, select, textarea');
  }

  Array.prototype.forEach.call(inputFields, function (elem) {
    if (!elem.dataset.retain) {
      if (elem.checked) {
        elem.checked = false;
      }
      elem.value = '';
    }
  });

  return inputFields;
};

// ### Form element manipulation

// Init JqueryUI datepicker widgets
var initDateFields = function () {
  'use strict';

  if (navigator.userAgent.match(/Firefox/)) {
    $('input[type="date"]').datepicker({
      dateFormat: 'yy-mm-dd'
    });
  }

  return true;
};

// Fill select options from a URL using Ajax
var fillOptionsFromUrl = function (url, selectElement, callback) {
  'use strict';

  ajax.get(url, function (req) {
    selectElement.innerHTML = templates['options'](req.response);
    if (callback) {
      callback();
    }
  });

  return selectElement;
};

exports.toggle = toggle;
exports.cancelDialog = cancelDialog;
exports.clear = clear;
exports.initDateFields = initDateFields;
exports.fillOptionsFromUrl = fillOptionsFromUrl;
exports.hide = hide;
exports.hideDisable = hideDisable;
exports.show = show;
exports.showEnable = showEnable;

},{"ajax":105}],79:[function(require,module,exports){
// # Formalize
//
// Convert JSON to and from an HTML form.

// ## Variable Definitions

var formalize_from = require('formalize_from');
var formalize_to = require('formalize_to');
// ## External Functions

var toForm = function (json, options) {
  'use strict';

  return formalize_to.transform(json, options);
};

var fromForm = function (html) {
  'use strict';

  return formalize_from.transform(html);
};

exports.toForm = toForm;
exports.fromForm = fromForm;

},{"formalize_from":131,"formalize_to":132}],80:[function(require,module,exports){
// # Form from
//
// Convert an HTML form to JSON.

// ## Variable Definitions

var htmlparser = require('htmlparser2');

// ## Internal Functions

// Reject non-strings or strings of length zero.
var validateFromArg = function (html) {
  'use strict';

  if (typeof html !== 'string') {
    throw 'invalid HTML: non-string';
  } else if (html.length === 0) {
    throw 'invalid HTML: ""';
  }

  return html;
};

var curr = function (state) {
  'use strict';

  return state.state[state.state.length - 1];
};

var showState = function (state) {
  'use strict';

  return ' [' + state.state.join(',') + ']';
};

var openForm = function (state) {
  'use strict';

  if (curr(state) === 'start') {
    state.state.push('open');
    state.acc = 'null';
  } else {
    throw 'invalid form: only one form allowed' + showState(state);
  }

  return state;
};

var addTextValue = function (state, text) {
  'use strict';

  state.acc = state.acc + '"' + text.replace(/\n/g, '\\n') + '"';

  return state;
};

var addKey = function (state, name) {
  'use strict';

  state.acc = addTextValue(state, name).acc + ':';

  return state;
};

var addComma = function (state) {
  'use strict';

  if (curr(state) === 'need-comma') {
    state.acc = state.acc + ',';
  } else {
    state.state.push('need-comma');
  }

  return state;
};

var openObject = function (state, attribs) {
  'use strict';

  if (state.acc === 'null') {
    state.acc = '';
  }

  addComma(state);

  if (attribs.title) {
    addKey(state, attribs.title);
  }

  state.state.push('open-object');

  state.acc = state.acc + '{';

  return state;
};

var openArray = function (state, attribs) {
  'use strict';

  addComma(state);

  if (attribs.title) {
    addKey(state, attribs.title);
  }

  state.state.push('open-array');

  state.acc = state.acc + '[';

  return state;
};

var addCorrectlyTypedValue = function (state, value) {
  'use strict';

  switch (value) {
  case 'null':
  case 'true':
  case 'false':
    state.acc = state.acc + value;
    break;
  default:
    addTextValue(state, value);
  }

  return state;
};

var addValue = function (state, attribs) {
  'use strict';

  addComma(state);

  if (attribs.name) {
    addKey(state, attribs.name);
  }

  if (attribs.type === 'text') {
    addCorrectlyTypedValue(state, attribs.value);
  } else if (attribs.type === 'number') {
    state.acc = state.acc + attribs.value;
  }

  return state;
};

var openTextareaValue = function (state, attribs) {
  'use strict';

  addComma(state);
  addKey(state, attribs.name);
  state.state.push('open-text');

  return state;
};

var addTextareaValue = function (state, str) {
  'use strict';

  if (curr(state) === 'open-text') {
    state.state.pop();
    addTextValue(state, str);
  }

  return state;
};

var noComma = function (state) {
  'use strict';

  if (curr(state) === 'need-comma') {
    state.state.pop();
  }

  return state;
};

var klose = function (state, targ, callback) {
  'use strict';

  var current;

  noComma(state);
  current = curr(state);

  if (current === targ) {
    callback(state);
  } else {
    throw 'invalid form: tag mismatch' + showState(state);
  }

  return state;
};

var closeForm = function (state) {
  'use strict';

  klose(state, 'open', function (state) {
    state.state = ['done'];

    return state;
  });

  return state;
};

var closeObject = function (state) {
  'use strict';

  klose(state, 'open-object', function (state) {
    state.state.pop();
    state.acc = state.acc + '}';

    return state;
  });

  return state;
};

var closeArray = function (state) {
  'use strict';

  klose(state, 'open-array', function (state) {
    state.state.pop();
    state.acc = state.acc + ']';

    return state;
  });

  return state;
};

// Main HTML parsing function. It uses the helper functions openForm,
// openObject, addValue and openTextareaValue.
var tryParseHTML = function (html) {
  'use strict';

  var state = {
    state: ['start'],
    acc: ''
  };
  var parser;

  parser = new htmlparser.Parser({
    onopentag: function (name, attribs) {
      switch (name) {
      case 'form':
        openForm(state);
        break;
      case 'ul':
        openObject(state, attribs);
        break;
      case 'ol':
        openArray(state, attribs);
        break;
      case 'input':
        addValue(state, attribs);
        break;
      case 'textarea':
        openTextareaValue(state, attribs);
        break;
      }
    },
    ontext: function (str) {
      if (!str.match(/^\s+$/)) {
        addTextareaValue(state, str);
      }
    },
    onclosetag: function (tagname) {
      switch (tagname) {
      case 'form':
        closeForm(state);
        break;
      case 'ul':
        closeObject(state);
        break;
      case 'ol':
        closeArray(state);
        break;
      }
    }
  });

  parser.write(html);
  parser.end();

  switch (state.state.pop()) {
  case 'start':
    throw 'invalid form: no form found' + showState(state);
  case 'open':
    throw 'invalid form: no closing tag' + showState(state);
  default:
    return state.acc;
  }
};

// ## External Functions

var transform = function (html) {
  'use strict';

  var json;

  validateFromArg(html);
  json = tryParseHTML(html);

  return json;
};

exports.transform = transform;

},{"htmlparser2":37}],81:[function(require,module,exports){
// # Form to
//
// Convert JSON to an HTML form.

// ## Variable Definitions

var json_to = require('lib/json_to');
var uuid = require('node-uuid');

// ## Internal Functions

var closeElement = function () {
  'use strict';

  return '</li>';
};

// Used for all form elements
var openElement = function (options) {
  'use strict';

  var id;

  if (options.noElementIds) {
    id = '';
  } else {
    id = ' id="' + uuid.v4() + '"';
  }

  return '<li' + id + '>';
};

// Return a label for a key.
var simpleKey = function (key, options) {
  'use strict';

  var retval = '';

  if (key) {
    if (options.spanLabel) {
      retval = '<span title="' + key + '" class="span-label">' + key + '</span>';
    } else {
      retval = '<label for="' + key + '">' + key + '</label>';
    }
  }

  return retval;
};

// Return a title for the key as a span element.
var spanTitle = function (key, options) {
  'use strict';

  var retval = '';
  var tc = options.titleClass ? ' ' + options.titleClass : '';

  if (key) {
    retval = '<span title="' + key + '" class="span-title' + tc + '">' + key + '</span>';
  } else if (options.arrayElementHandles) {
    retval = '<span class="array-element-handle">' + options.arrayElementHandles + '</span>';
  }

  return retval;
};

// For a complex type, such as an object or array.
var complex = function (item, options) {
  'use strict';

  var id;
  var listType = 'ul';

  if (item.type === 'array') {
    listType = 'ol';
  }

  if (options.noObjectIds) {
    id = '';
  } else {
    id = ' id="' + uuid.v4() + '" ';
  }

  return [openElement(options) + spanTitle(item.key, options) + '<' + listType + id + (item.key ? ' title="' + item.key + '"' : '') + '>', '</' + listType + '>' + closeElement(options)];
};

// Longer text input.
var textarea = function (key, value, options) {
  'use strict';

  var id;

  if (options.noInputIds) {
    id = '';
  } else {
    id = ' id="' + uuid.v4() + '"';
  }

  return '<textarea' + id + (key ? 'name="' + key + '"' : '') + '>' + value + '</textarea>';
};

// Could be text or number.
var inputarea = function (key, value, type, options) {
  'use strict';

  var id;

  if (options.noInputIds) {
    id = '';
  } else {
    id = ' id="' + uuid.v4() + '"';
  }

  return '<input' + id + ' type="' + (type === 'number' ? 'number' : 'text') + '" ' + (key ? 'name="' + key + '" ' : '') + 'value="' + value + '"/>';
};

var simple = function (item, options) {
  'use strict';

  var retval = openElement(options) + simpleKey(item.key, options);

  if (item.type === 'string' && item.value.length > 32) {
    retval = retval + textarea(item.key, item.value, options);
  } else {
    retval = retval + inputarea(item.key, item.value, item.type, options);
  }

  return [retval + closeElement(options), ''];
};

var context = function (options) {
  'use strict';

  var retval = ['', ''];

  if (!options.noForm) {
    retval = ['<form>', '</form>'];
  }

  return retval;
};

var root = function () {
  'use strict';

  return ['<ul>', '</ul>'];
};

// ## External Functions

var transform = function (json, options) {
  'use strict';

  var funs = {
    context: context,
    root: root,
    simple: simple,
    complex: complex
  };

  return json_to.transform(json, funs, options);
};

exports.transform = transform;

},{"lib/json_to":152,"node-uuid":51}],82:[function(require,module,exports){
// # Globals object
//
// A place to temporarily store global objects. Sometimes this is more
// convenient than using other types of client side storage. It is used
// rarely and explicitly using this object.
//
// This is not loaded as a module like other code here. It is concatenated
// to the beginning of the target JavaScript file created by the build
// process.

var globals = {};

},{}],83:[function(require,module,exports){
// # Builder dialog
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Dialog for adding conditions to user created indexes.

// Variable Definitions

var ihelpers = require('index_tool/ihelpers');
var form = require('form');
var ajax = require('ajax');
var evs = require('index_tool/ievents');

// Exported functions

// The dialog for adding a condition to an index.
var initIndexBuilderDialog = function (indexDoctype) {
  'use strict';

  var builderOrInput = document.getElementById('builder-or-input');
  var builderParenInput = document.getElementById('builder-paren-input');
  var builderNegateInput = document.getElementById('builder-negate-input');
  var builderOperatorInput = document.getElementById('builder-operator-input');
  var builderArgumentInput = document.getElementById('builder-argument-input');
  var builderFieldsetInput = document.getElementById('builder-fieldset-input');
  var builderFieldInput = document.getElementById('builder-field-input');
  var builderConditions = document.getElementById('builder-conditions');
  var builderParens = document.getElementById('builder-parens');
  var builderOr = document.getElementById('builder-or');
  var dialogElem = document.getElementById('index-builder-dialog');
  var tableBody = document.getElementById('index-conditions-listing').getElementByTagName('tbody');
  var notBlank = [builderOperatorInput, builderFieldsetInput, builderFieldInput];
  var fieldset_url = 'doctypes/' + indexDoctype + '/fieldsets';
  var condition_url = 'indexes/condition';

  builderOperatorInput.setAttribute('disable', 'disable');
  builderArgumentInput.setAttribute('disable', 'disable');
  builderFieldsetInput.setAttribute('disable', 'disable');
  builderFieldInput.setAttribute('disable', 'disable');
  document.querySelector('.ui-helper-reset div').classList.remove('hidden');

  var appendCondition = function (builderRow) {
    tableBody.insertAdjacentHTML('beforeend', builderRow);
    // TODO: allow for arranging rows some other way.
    $(tableBody).sortable();

    return false;
  };

  ihelpers.fOpts(fieldset_url, builderFieldsetInput, function () {
    builderFieldsetInput.removeAttribute('disable');
  });

  builderOrInput.onchange = function () {
    if (builderOrInput.checked) {
      builderConditions.classList.add('hidden');
      builderParens.classList.add('hidden');
    } else {
      builderConditions.classList.remove('hidden');
      builderParens.classList.remove('hidden');
    }
  };

  builderParenInput.onchange = function () {
    if (builderParenInput.value) {
      builderConditions.classList.add('hidden');
      builderOr.classList.add('hidden');
    } else {
      builderConditions.classList.remove('hidden');
      builderOr.classList.remove('hidden');
    }
  };

  var fieldsetEvents = function () {
    evs.setIndexFieldsetEvents(indexDoctype, builderFieldsetInput, builderFieldInput, function () {
      builderOperatorInput.setAttribute('disable', 'disable');
      builderFieldInput.setAttribute('disable', 'disable');
      builderArgumentInput.setAttribute('disable', 'disable');

      return function () {
        builderFieldInput.removeAttribute('disable');
      };
    });
  };

  var fieldEvents = function () {
    evs.setIndexFieldEvents(indexDoctype, builderFieldsetInput, builderFieldInput, function () {
      builderOperatorInput.setAttribute('disable', 'disable');
      builderArgumentInput.setAttribute('disable', 'disable');

      return function () {
        builderOperatorInput.removeAttribute('disable');
      };
    });
  };

  var operatorEvents = function () {
    evs.setIndexOperatorEvents(builderArgumentInput, builderOperatorInput, builderFieldInput, function () {
      builderArgumentInput.setAttribute('disable', 'disable');

      return function () {
        builderArgumentInput.removeAttribute('disable');
      };
    });
  };

  var dialog = $(dialogElem).dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      'Create': function () {
        Array.prototype.forEach.call(document.querySelectorAll('.input'), function (item) {
          item.classList.remove('ui-state-error');
        });

        // place holder for client side validation
        var checkResult = true;

        if (!builderOrInput.checked && !builderParenInput.value) {
          notBlank.forEach(function (item) {
            if (item.value.isBlank()) {
              item.classList.add('ui-state-error');
              checkResult = false;
            } else {
              item.classList.remove('ui-state-error');
            }
          });
        }

        if (checkResult) {
          if (builderOrInput.checked) {
            ajax.get(condition_url + '?is_or=true', function (req) {
              appendCondition(req);
            });
          } else if (builderParenInput.value) {
            ajax.get(condition_url + '?is_or=false&parens=' + builderParenInput.value + '&negate=false', function (req) {
              appendCondition(req);
            });
          } else {
            ajax.get(condition_url + '?is_or=false&parens=false&negate=' + builderNegateInput.checked.toString() + '&fieldset=' + builderFieldsetInput.value + '&field=' + builderFieldInput.value + '&operator=' + builderOperatorInput.value + '&argument=' + builderArgumentInput.value, function (req) {
              appendCondition(req);
            });
          }

          $(this).dialog('close');
        }
      },
      'Cancel': function () {
        $(this).dialog('close');
      }
    },
    close: function () {
      builderConditions.classList.remove('hidden');
      builderFieldsetInput.onchange = undefined;
      builderFieldInput.onchange = undefined;
      builderOperatorInput.onchange = undefined;
      form.clear(document.getElementsByClassName('input'));
      Array.prototype.forEach.call(document.getElementsByClassName('input'), function (item) {
        item.classList.remove('ui-state-error');
      });
    }
  });

  fieldsetEvents();
  fieldEvents();
  operatorEvents();

  return dialog;
};

exports.initIndexBuilderDialog = initIndexBuilderDialog;

},{"ajax":105,"form":129,"index_tool/ievents":135,"index_tool/ihelpers":136}],84:[function(require,module,exports){
// # The file manager
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Interface for manipulating index conditions.

// Variable Definitions

var initIndexNewDialog = require('index_tool/new-dialog').initIndexNewDialog;
var initIndexBuilderDialog = require('index_tool/builder-dialog').initIndexBuilderDialog;
var initReplaceDialog = require('index_tool/replace-dialog').initReplaceDialog;
var ilistingui = require('index_tool/ilistingui');
var ipreviewui = require('index_tool/ipreviewui');
var ihelpers = require('index_tool/ihelpers');
var ajax = require('ajax');
var flash = require('flash');

// Internal functions

// User interface element
var tableBody = function () {
  'use strict';

  return document.getElementById('index-conditions-listing').getElementByTagName('tbody');

};

// User interface element
var editingData = function () {
  'use strict';

  return document.getElementById('index-editing-data');
};

// Make sure the arguments are of the correct type.
var fixArgumentType = function (argument, subcategory, operator) {
  'use strict';

  switch (subcategory) {
  case 'integer':
  case 'rational':
    argument = argument * 1;
    break;
  }

  switch (operator) {
  case 'hasExactly':
  case 'hasGreater':
  case 'hasLess':
    argument = Math.floor(argument * 1);
    break;
  }

  return argument;
};

// Use data in `data` attributes of HTML elements to produce an array
// of conditions.
var getIndexConditions = function (doctypeId, rows) {
  'use strict';

  var conditions = Array.prototype.map.call(rows, function (row) {
    var is_or = row.querySelector('td.or-condition').dataset.value === 'true';
    var paren = row.querySelector('td.paren-condition').dataset.value;
    var condition;

    if (is_or) {
      condition = {
        'is_or': true,
        'parens': false
      };
    } else if (paren) {
      condition = {
        'is_or': false,
        'parens': paren
      };
    } else {
      var fieldId = row.querySelector('td.field-condition').dataset.value;
      var fieldsetId = row.querySelector('td.fieldset-condition').dataset.value;
      var argument = row.querySelector('td.argument-condition').dataset.value;
      var fieldDoc = ihelpers.getFieldDoc(fieldId, fieldsetId, doctypeId);
      var negate = row.querySelector('td.negate-condition').dataset.value === 'true';
      var operator = row.querySelector('td.operator-condition').dataset.value;

      argument = fixArgumentType(argument, fieldDoc.subcategory, operator);

      condition = {
        'is_or': false,
        'parens': false,
        'negate': negate,
        'fieldset': fieldsetId,
        'field': fieldId,
        'operator': operator,
        'argument': argument
      };
    }

    return condition;
  });

  return conditions;
};

// Initiate the save action.
var saveIndex = function (buttonData, completeFunction) {
  'use strict';

  var indexId = buttonData.dataset.indexId;
  var indexRev = buttonData.dataset.indexRev;
  var url = 'indexes/' + indexId + '?rev=' + indexRev;
  var doctype = buttonData.dataset.indexDoctype;

  var obj = {
    '_id': indexId,
    'category': 'index',
    'doctype': doctype,
    'show_deleted': buttonData.dataset.indexShow_deleted === 'true',
    'fields': JSON.parse(buttonData.dataset.indexFields),
    'fields_label': JSON.parse(buttonData.dataset.indexFields_label),
    'name': buttonData.dataset.indexName,
    'conditions': getIndexConditions(doctype, document.querySelectorAll('#index-conditions-listing tbody tr'))
  };

  if (buttonData.dataset.indexReplace_function) {
    obj.replace_function = buttonData.dataset.indexReplace_function;
  }

  ajax.put(url, obj, 'PUT', completeFunction);

  return false;
};

// Initiate the delete action.
var deleteIndex = function (indexId, indexRev, completeMessage, completeFunction) {
  'use strict';

  var url = 'indexes/' + indexId + '?rev=' + indexRev;
  var title;
  var body;
  var statusCallbacks = [];

  statusCallbacks[204] = function () {
    title = 'Success';
    body = completeMessage;

    completeFunction();

    flash.highlight(title, body);
  };
  statusCallbacks[409] = function (req) {
    body = req.response.message;
    title = req.statusText;

    flash.error(title, body);
  };
  statusCallbacks[404] = function (req) {
    body = 'Index appears to have been deleted already.';
    title = req.statusText;

    flash.error(title, body);
  };

  ajax.del(url, undefined, statusCallbacks);

  return false;
};

// Exported functions

// Initialize the index editing user interface.
var init = function (target) {
  'use strict';

  var indexId = target.dataset.indexId;
  var url = 'indexes/' + indexId;
  var htmlTarget = document.getElementById('index-conditions');

  ajax.get(url, function (req) {
    htmlTarget.innerHTML = templates['index-conditions'](req.response);
    $(tableBody()).sortable();
    ipreviewui.get();
  });

  return false;
};

// Save the index.
var save = function () {
  'use strict';

  var bData = editingData();

  if (bData.length !== 0) {
    var completeFunction = function () {
      init(bData);
      flash.highlight('Success', 'Your index has been saved.');
    };

    saveIndex(bData, completeFunction);
  } else {
    flash.highlight('Info', 'No index has been chosen to save.');
  }
};

// Open the replace dialog, which allows the user to enter a function
// that will replace the normal output of the index.
var replace = function () {
  'use strict';

  var bData = editingData();

  if (bData.length !== 0) {
    initReplaceDialog.dialog('open');
  } else {
    flash.highlight('Info', 'You must choose an index first.');
  }

  return true;
};

// Add a condition using the index builder dialog.
var addCond = function () {
  'use strict';

  var bData = editingData();

  if (bData.length !== 0) {
    initIndexBuilderDialog(bData.dataset.indexDoctype).dialog('open');
  } else {
    flash.highlight('Info', 'You must choose an index first.');
  }

  return true;
};

// Handle the mouse click initiate action of removing a condition.
var remCond = function (target) {
  'use strict';

  //$(target).closest('tr').remove();
  throw 'intentional error';
};

// Open the new index dialog.
var newCond = function () {
  'use strict';

  initIndexNewDialog().dialog('open');
  return true;
};

// Delete the current index.
var del = function () {
  'use strict';

  var bData = editingData();

  if (bData.length !== 0) {
    var indexId = bData.dataset.indexId;
    var indexRev = bData.dataset.indexRev;
    var completeMessage = 'Your index has been deleted.';
    var completeFunction = function () {
      document.getElementById('index-conditions').innerHTML = '';
      ilistingui.init();
    };

    if (window.confirm('Are you sure?')) {
      deleteIndex(indexId, indexRev, completeMessage, completeFunction);
    }
  } else {
    flash.highlight('Info', 'No index has been chosen to delete.');
  }

  return true;
};

exports.init = init;
exports.save = save;
exports.replace = replace;
exports.addCond = addCond;
exports.remCond = remCond;
exports.newCond = newCond;
exports.del = del;

},{"ajax":105,"flash":128,"index_tool/builder-dialog":133,"index_tool/ihelpers":136,"index_tool/ilistingui":137,"index_tool/ipreviewui":138,"index_tool/new-dialog":139,"index_tool/replace-dialog":140}],85:[function(require,module,exports){
// # Dialog Events
//
// *Implicit depends:* DOM
//
// These are change events triggered in the dialogs.

// ## Variable Definitions

var h = require('index_tool/ihelpers');

// ## Exported Functions

// Set change events for the operator field.
var setIndexOperatorEvents = function (argumentField, operatorField, fieldField, callback) {
  'use strict';

  operatorField.onchange = function () {
    var callback2;

    if (callback) {
      callback2 = callback();
    }

    h.alterArg(argumentField, operatorField, fieldField, callback2);
  };

  return true;
};

exports.setIndexOperatorEvents = setIndexOperatorEvents;

},{"index_tool/ihelpers":136}],86:[function(require,module,exports){
// # Index tool helpers.
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Shared functions used by a number of index tool modules.

// Variable Definitions

var s = require('sess');
var ajax = require('ajax');
var form = require('form');

// Internal functions

// Disable certain options match `disables`.
var disableOptions = function (options, disables) {
  'use strict';

  Array.prototype.forEach.call(options.childNodes, function (node) {
    node.classList.remove('hidden');
  });

  disables.forEach(function (item) {
    options.querySelector('option:contains(' + item + ')').classList.add('hidden');
  });

  return false;
};

// Disable the operator options.
var disableOperatorOptions = function (fieldDoc) {
  'use strict';

  var options = document.getElementById('builder-operator-input');

  switch (fieldDoc.subcategory) {
  case 'select':
  case 'docselect':
  case 'text':
  case 'textarea':
    disableOptions(options, ['member', 'true']);
    break;
  case 'integer':
  case 'rational':
  case 'date':
    disableOptions(options, ['member', 'true', 'match']);
    break;
  case 'boolean':
  case 'openboolean':
    disableOptions(options, ['equal', 'greater', 'less', 'member', 'match']);
    break;
  case 'multiselect':
  case 'docmultiselect':
    disableOptions(options, ['equal', 'greater', 'less', 'true', 'match']);
    break;
  }

  return false;
};

// Exported functions

// Handles an input field that presents different behavior depending on
// the values of previously filled in fields.
var alterArg = function (argumentField, operatorField, fieldField, callback) {
  'use strict';

  var fieldDoc = function () {
    return s.get(fieldField.value);
  };

  callback();

  // TODO: remove Jquery UI dep.
  try {
    // Destroy these if initialized already
    $(argumentField).removeAttr('disabled').datepicker('destroy');
    $(argumentField).removeAttr('disabled').autocomplete('destroy');
  } catch (err) {
    window.console.log(err.message);
  }

  var dateOrText = function (argumentField, fdoc) {
    if (fdoc.subcategory === 'date') {
      $(argumentField).removeAttr('disabled').datepicker({
        dateFormat: 'yy-mm-dd'
      });
    } else {
      $(argumentField).removeAttr('disabled').autocomplete({
        source: fdoc.allowed
      });
    }

    return true;
  };

  var fdoc = fieldDoc();

  if (fdoc) {
    switch (operatorField.value) {
    case 'true':
    case 'isDefined':
    case 'blank':
      argumentField.setAttribute('disabled', 'disabled').value = '';
      break;
    case 'equal':
    case 'member':
    case 'greater':
    case 'less':
    case 'hasExactly':
    case 'hasGreater':
    case 'hasLess':
      dateOrText(argumentField, fdoc);
      break;
    }

  }

  return true;
};

// Certain operator options only exist for certain types of fields.
var alterOpts = function (fieldDoc, fieldId, callback) {
  'use strict';

  disableOperatorOptions(fieldDoc);
  callback();

  return true;
};

// Get the document holding the field information.
var getFieldDoc = function (fieldId, fieldsetId, doctypeId, callback) {
  'use strict';

  var fieldDoc = s.get(fieldId);
  var url = 'doctypes/' + doctypeId + '/fieldsets/' + fieldsetId + '/fields/' + fieldId + '?format=json';

  if (fieldDoc) {
    if (callback) {
      callback(fieldDoc);
    }
    return fieldDoc;
  } else {
    ajax.get(url, function (req) {
      s.put(req.response);
      if (callback) {
        callback(s.get(fieldId));
      }
    });

    return s.get(fieldId);
  }
};

// Return an object containing methods for working with common events.
var evs = function () {
  'use strict';

  var mod = {};

  mod.setIndexDoctypeEvents = function (indexDoctype, indexFieldset, callback) {
    indexDoctype.change(function () {
      var url = 'doctypes/' + indexDoctype.val() + '/fieldsets';
      var callback2;

      if (callback) {
        callback2 = callback();
      }

      form.fillOptionsFromUrl(url, indexFieldset, callback2);
    });

    return false;
  };

  mod.setIndexFieldsetEvents = function (indexDoctype, indexFieldset, indexField, callback) {
    indexFieldset.onchange = function () {
      var callback2;

      if (typeof indexDoctype !== 'string') {
        indexDoctype = indexDoctype.value;
      }

      if (indexFieldset.value) {
        var url = 'doctypes/' + indexDoctype + '/fieldsets/' + indexFieldset.value + '/fields?as=options';

        if (callback) {
          callback2 = callback();
        }

        form.fillOptionsFromUrl(url, indexField, callback2);
      }
    };

    return mod;
  };

  mod.setIndexFieldEvents = function (indexDoctype, indexFieldset, indexField, callback) {
    indexField.onchange = function () {
      var fieldId = indexField.value;
      var fieldsetId = indexFieldset.value;
      var callback2;

      if (callback) {
        callback2 = callback();
      }

      if (!(fieldId.isBlank())) {
        getFieldDoc(fieldId, fieldsetId, indexDoctype, function (data) {
          alterOpts(data, fieldId, callback2);
        });
      }
    };

    return mod;
  };

  mod.setIndexOperatorEvents = function (argumentField, operatorField, fieldField, callback) {
    operatorField.onchange = function () {
      var callback2;

      if (callback) {
        callback2 = callback();
      }

      alterArg(argumentField, operatorField, fieldField, callback2);
    };

    return mod;
  };
};

exports.alterArg = alterArg;
exports.alterOpts = alterOpts;
exports.getFieldDoc = getFieldDoc;
exports.evs = evs;

},{"ajax":105,"form":129,"sess":147}],87:[function(require,module,exports){
// # Index listing.
//
// *Implicit depends:* DOM
//
// Displays a listing of user created indexes.

// Variable Definitions

var templates = require('templates');
var ajax = require('ajax');

// Exported functions

// Initialize the listing of user created indexes.
var init = function () {
  'use strict';

  var url = 'indexes';
  var target = document.getElementById('index-index-listing');
  var listing;

  ajax.get(url, function (req) {
    listing = templates['index-listing'](req.response);
    target.innerHTML = listing;
  });

  return true;
};

exports.init = init;

},{"ajax":105,"templates":52}],88:[function(require,module,exports){
// # Paging For Index Listing
//
// *Implicit depends:* DOM, JSON
//
// Loads sample of the user index based on user suplied values.

// Variable Definitions

var pager = require('pager').pager;

// Exported Functions

// Return the 'prefix' which is used in id and class names for
// elements used to page through these values.
var prefix = function () {
  'use strict';

  return 'preview';
};

// Called by a keystroke event handler when user changes form values.
var get = function () {
  'use strict';

  var indexId = document.getElementById('index-editing-data').getAttribute('data-index-id');
  var url = 'indexes/' + indexId + '/preview';
  var target = document.getElementById(prefix() + '-list-view');

  var format = function (resp) {
    resp.rows = resp.rows.map(function (item) {
      item.display_key = item.key.map(function (k) {
        return k[1];
      });

      return item;
    });

    return resp;
  };

  if (indexId) {
    pager({
      prefix: prefix(),
      format: format,
      url: url,
      target: target
    }).get();
  }

  return true;
};

exports.prefix = prefix;
exports.get = get;

},{"pager":143}],89:[function(require,module,exports){
// # New dialog
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Dialog for adding a new user created index.

// ## Variable Definitions

var ihelpers = require('index_tool/ihelpers');
var ilistingui = require('index_tool/ilistingui');
var ajax = require('ajax');
var form = require('form');

// ## Internal Functions

var indexDoctype = function () {
  'use strict';

  return document.getElementById('index-doctype-input');
};

var indexFieldset = function () {
  'use strict';

  return document.getElementById('index-fieldset-input');
};

var indexField = function () {
  'use strict';

  return document.getElementById('index-field-input');
};

var handleChange = function (changed, dependent) {
  'use strict';

  if (changed.value && !changed.value.isBlank()) {
    dependent[0].removeAttribute('disabled');

    Array.prototype.forEach.call(dependent[0].getElementsByTagName('option'), function (item) {
      if (item.classList.contains(changed.value)) {
        item.classList.remove('hidden');
        item.removeAttribute('disabled');
      } else {
        item.classList.add('hidden');
        item.setAttribute('disabled', 'disabled');
      }
    });
  } else {
    dependent.forEach(function (item) {
      item.value = '';
      item.setAttribute('disabled', 'disabled');
    });
  }

  return changed;
};

var getLabelForVal = function (val) {
  'use strict';

  return document.querySelector('#index-new-dialog option[value="' + val + '"]').innerHTML;
};

var getLabel = function (indexFieldset, indexField) {
  'use strict';

  return [getLabelForVal(indexFieldset.value), getLabelForVal(indexField.value)].join(':');
};

// ## Exported Functions

// The dialog for adding a new index.
var initIndexNewDialog = function () {
  'use strict';

  var indexName = document.getElementById('index-name-input');
  var indexShowDeleted = document.getElementById('index-show_deleted-input');

  indexFieldset().setAttribute('disabled', 'disabled');
  indexField().setAttribute('disabled', 'disabled');

  var dialog = $('#index-new-dialog').dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      'Create': function () {
        Array.prototype.forEach.call(document.querySelectorAll('.input'), function (item) {
          item.classList.remove('ui-state-error');
        });

        // place holder for client side validation
        var checkResult = true;

        if (checkResult) {
          var obj = {
            'category': 'index',
            'name': indexName.value,
            'show_deleted': indexShowDeleted.checked,
            'conditions': [],
            'doctype': indexDoctype().value,
            'fields_label': [getLabel(indexFieldset(), indexField())],
            'fields': [indexField().value]
          };
          var complete = function () {
            ilistingui.init();
            dialog.dialog('close');
          };

          ajax.post('indexes', obj, complete);
        }
      },
      'Cancel': function () {
        $(this).dialog('close');
      }
    },
    close: function () {
      var cleared = form.clear(document.querySelectorAll('.input'));
      Array.prototype.forEach.call(cleared, function (item) {
        item.classList.remove('ui-state-error');
      });
    }
  });

  return dialog;
};

var doctypeInputChange = function () {
  'use strict';

  return handleChange(indexDoctype(), [indexFieldset(), indexField()]);
};

var fieldsetInputChange = function () {
  'use strict';

  return handleChange(indexFieldset(), [indexField()]);
};

exports.doctypeInputChange = doctypeInputChange;
exports.fieldsetInputChange = fieldsetInputChange;
exports.initIndexNewDialog = initIndexNewDialog;

},{"ajax":105,"form":129,"index_tool/ihelpers":136,"index_tool/ilistingui":137}],90:[function(require,module,exports){
// # Replace dialog
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Dialog for providing a function to replace the normal output of
// an index.

// Variable Definitions

var ihelpers = require('index_tool/ihelpers');

// Exported functions

// The dialog for providing a function to replace the normal output of
// an index.
var initReplaceDialog = function () {
  'use strict';

  var replaceFunction = document.getElementById('index-replace_function-input');
  var indexData = document.getElementById('index-editing-data');
  var remove = document.getElementById('index-remove_function-input');
  var dialogElem = document.getElementById('index-replace-dialog');
  var message = document.getElementById('replace-function-message');

  if (indexData.dataset.indexReplace_function) {
    replaceFunction.value = indexData.dataset.indexReplace_function;
  } else {
    replaceFunction = '';
    replaceFunction.classList.remove('ui-state-error');
  }

  var dialog = $(dialogElem).dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      'Save': function () {

        Array.prototype.forEach.call(document.getElementsByClassName('input'), function (item) {
          item.classList.remove('ui-state-error');
        });

        // place holder for client side validation
        var checkResult = true;

        if (!remove.checked) {
          if (replaceFunction.value.isBlank()) {
            replaceFunction.classList.add('ui-state-error');
          } else {
            replaceFunction.classList.remove('ui-state-error');
          }

          if (checkResult) {
            indexData.dataset.indexReplace_function = replaceFunction.value;
            message.innerHTML = 'This index has a replacement function.';
          }
        } else {
          indexData.removeAttr('data-index-replace_function');
          message.innerHTML = '';
        }

        $(this).dialog('close');
      },
      'Cancel': function () {
        $(this).dialog('close');
      }
    },
    close: function () {
      replaceFunction.value = '';
      replaceFunction.classList.remove('ui-state-error');
    }
  });

  return dialog;
};

exports.initReplaceDialog = initReplaceDialog;

},{"index_tool/ihelpers":136}],91:[function(require,module,exports){
/*
 * jQuery Hotkeys Plugin
 * Copyright 2010, John Resig
 * Modified by Noah Diewald
 * Dual licensed under the MIT or GPL Version 2 licenses.
 *
 * Based upon the plugin by Tzury Bar Yochay:
 * http://github.com/tzuryby/hotkeys
 *
 * Original idea by:
 * Binny V A, http://www.openjs.com/scripts/events/keyboard_shortcuts/
 */

(function (jQuery) {
  'use strict';

  jQuery.hotkeys = {
    version: '0.8',

    specialKeys: {
      8: 'backspace',
      9: 'tab',
      13: 'return',
      16: 'shift',
      17: 'ctrl',
      18: 'alt',
      19: 'pause',
      20: 'capslock',
      27: 'esc',
      32: 'space',
      33: 'pageup',
      34: 'pagedown',
      35: 'end',
      36: 'home',
      37: 'left',
      38: 'up',
      39: 'right',
      40: 'down',
      45: 'insert',
      46: 'del',
      96: '0',
      97: '1',
      98: '2',
      99: '3',
      100: '4',
      101: '5',
      102: '6',
      103: '7',
      104: '8',
      105: '9',
      106: '*',
      107: '+',
      109: '-',
      110: '.',
      111: '/',
      112: 'f1',
      113: 'f2',
      114: 'f3',
      115: 'f4',
      116: 'f5',
      117: 'f6',
      118: 'f7',
      119: 'f8',
      120: 'f9',
      121: 'f10',
      122: 'f11',
      123: 'f12',
      144: 'numlock',
      145: 'scroll',
      191: '/',
      224: 'meta'
    },

    shiftNums: {
      '`': '~',
      '1': '!',
      '2': '@',
      '3': '#',
      '4': '$',
      '5': '%',
      '6': '^',
      '7': '&',
      '8': '*',
      '9': '(',
      '0': ')',
      '-': '_',
      '=': '+',
      ';': ': ',
      '\'': '"',
      ',': '<',
      '.': '>',
      '/': '?',
      '\\': '|'
    }
  };

  function keyHandler(handleObj) {
    // Only care when a possible input has been specified
    if (typeof handleObj.data !== 'string') {
      return;
    }

    var origHandler = handleObj.handler,
      keys = handleObj.data.toLowerCase().split(' ');

    handleObj.handler = function (event) {
      // Don't fire in text-accepting inputs that we didn't directly bind to
      // MODIFIED FROM ORIGINAL
      //if ( this !== event.target && (/textarea|select/i.test( event.target.nodeName ) ||
      //      event.target.type === 'text') ) {
      //	return;
      //}
      // Keypress represents characters, not special keys
      var special = event.type !== 'keypress' && jQuery.hotkeys.specialKeys[event.which],
        character = String.fromCharCode(event.which).toLowerCase(),
        key, modif = '',
        possible = {};

      // check combinations (alt|ctrl|shift+anything)
      if (event.altKey && special !== 'alt') {
        modif += 'alt+';
      }

      if (event.ctrlKey && special !== 'ctrl') {
        modif += 'ctrl+';
      }

      // TODO: Need to make sure this works consistently across platforms
      if (event.metaKey && !event.ctrlKey && special !== 'meta') {
        modif += 'meta+';
      }

      if (event.shiftKey && special !== 'shift') {
        modif += 'shift+';
      }

      if (special) {
        possible[modif + special] = true;

      } else {
        possible[modif + character] = true;
        possible[modif + jQuery.hotkeys.shiftNums[character]] = true;

        // '$' can be triggered as 'Shift+4' or 'Shift+$' or just '$'
        if (modif === 'shift+') {
          possible[jQuery.hotkeys.shiftNums[character]] = true;
        }
      }

      for (var i = 0, l = keys.length; i < l; i++) {
        if (possible[keys[i]]) {
          return origHandler.apply(this, arguments);
        }
      }
    };
  }

  jQuery.each(['keydown', 'keyup', 'keypress'], function () {
    jQuery.event.special[this] = {
      add: keyHandler
    };
  });

})(jQuery);

},{}],92:[function(require,module,exports){
// # Change Event Handling
//
// *Implicit depends:* DOM, JQuery, JQueryUI
//
// Like [`click-dispatch.js`](./click-dispatch.html) I would like
// to centralize the keystroke events. This is a start and a bit of
// an experiment. It uses the JQuery `on()` function, which was not
// available when I first began programming this application. It also
// uses the JQuery hotkeys plugin, which I'd like to remove at some point.

// ## Variable Definitions

var hotkeys = require('jquery.hotkeys');
var S = require('sender');
var ipreviewui = require('index_tool/ipreviewui');
var indexui = require('documents/indexui');
var changeui = require('documents/changeui');
var editui = require('documents/editui');
var viewui = require('documents/viewui');
var searchui = require('documents/searchui');
var charsequi = require('config/charsequi');
var doctypeui = require('config/doctypeui');

// # Exported Functions

// All this does is register a bunch of event handlers.
var keystrokes = function () {
  'use strict';

  [ipreviewui, indexui, changeui, doctypeui, charsequi].forEach(function (mod) {
    var keyupHandler = function (e) {
      var getIndexTimer;
      window.clearTimeout(getIndexTimer);
      getIndexTimer = setTimeout(function () {
        if (e.which !== 8 && e.which !== 46) {
          mod.get();
        }
      }, 500);
    };

    document.addEventListener('keyup', function (e) {
      if (e.target.id === mod.prefix() + '-filter') {
        keyupHandler(e);
      } else if (e.target.id === mod.prefix() + '-limit') {
        keyupHandler(e);
      }
    });
  });

  $(document).on('keydown', '#document-worksheets-form', function (e) {
    if (e.which === 13) {
      S.sender('worksheet-form-submit');
      return false;
    }
    return true;
  });

  $(document).on('keydown', '#document-sets-form', function (e) {
    if (e.which === 13) {
      S.sender('sets-form-submit');
      return false;
    }
    return true;
  });

  $('#new-set-form').on('keydown', function (e) {
    if (e.which === 13) {
      S.sender('new-set-form-submit');
      return false;
    }
    return true;
  });

  $(document).bind('keydown', 'Alt+n', function (e) {
    var t = function () {
      return $('#edit-tabs');
    };
    var totaltabs = t().find('li').length;
    var selected = t().tabs('option', 'active');

    if (selected < totaltabs - 1) {
      t().tabs('option', 'active', selected + 1);
      S.sender('lost-focus');
    } else {
      t().tabs('option', 'active', 0);
      S.sender('lost-focus');
    }

    return false;
  });

  $(document).bind('keydown', 'Alt+c', function (e) {
    var active = $(document.activeElement).attr('id');
    S.sender('initiated-command', active);
    return true;
  });

  $(document).bind('keydown', 'Alt+p', function (e) {
    var t = function () {
      return $('#edit-tabs');
    };
    var totaltabs = t().find('li').length;
    var selected = t().tabs('option', 'active');

    if (selected !== 0) {
      t().tabs('option', 'active', selected - 1);
      S.sender('lost-focus');
    } else {
      t().tabs('option', 'active', totaltabs - 1);
      S.sender('lost-focus');
    }

    return false;
  });


  $(document).on('keydown', '#edit-command-input', function (e) {
    if (e.which === 13) {
      var command = $('#edit-command-input').val();
      S.sender('submitted-command', command);
    }
    return true;
  });

  $(document).on('keydown', '#edit-document-form input', function (e) {
    if (e.which === 13) {
      if ($('#save-document-button').css('display') === 'none') {
        editui.create();
      } else {
        editui.save();
      }
    }
    return true;
  });

  $(document).on('keydown', '#edit-document-form textarea', 'Alt+x', function (e) {
    editui.toggleTextarea(e.target);
    return false;
  });

  $(document).on('keypress', '#view-jump-id', function (e) {
    if (e.which === 13) {
      var docid = $('#view-jump-id').val();
      viewui.get(docid);
      return false;
    }
    return true;
  });

  $(document).on('keydown', '#document-search-term', function (e) {
    if (e.which === 13) {
      searchui.getSearch();
      return false;
    }
    return true;
  });

  return true;
};

exports.keystrokes = keystrokes;

},{"config/charsequi":108,"config/doctypeui":110,"documents/changeui":115,"documents/editui":118,"documents/indexui":120,"documents/searchui":122,"documents/viewui":125,"index_tool/ipreviewui":138,"jquery.hotkeys":141,"sender":146}],93:[function(require,module,exports){
// # Paging List-like Info
//
// *Implicit depends:* DOM, JSON
//
// This is basically semi-generic paging code.
//
// Get the index that is displayed in the index pane.  startkey and
// startid map directly to the same concepts in couchdb view queries. The
// prevkeys and previds are used to hold information that will allow
// the user to page backward through the listing. They are arrays of
// keys and ids corresponding to previous page's startkeys and ids.
//
// There are a number of values that this function depends on that
// are taken from the HTML. These include the value for the limit and
// the nextkey and nextid for paging forward. Also the current key and
// id are taken from the html when needed to add to the prevkeys and
// previds. The startkey may be a user input value so a more reliable
// startkey and startid are needed.

// Variable Definitions

var templates = require('templates');
var ajax = require('ajax');

// Exported functions

// Initialize the pager with an args object.
var pager = function (args) {
  'use strict';

  var mod = {};
  // If the 'prefix' used to automatically determine certain element
  // ID's is not set, set it to 'index'.
  if (args.prefix === undefined) {
    args.prefix = 'index';
  }
  // Special formatting or template code.
  var format = args.format;
  var prefix = args.prefix;

  // Escape a value and base64 encode it.
  var escapeValue = function (value) {
    return window.btoa(window.unescape(window.encodeURIComponent(JSON.stringify(value))));
  };

  // The number of elements to display is given here. Note how `prefix`
  // is used.
  var limitField = function () {
    return document.getElementById(prefix + '-limit');
  };

  // Get the first or next page. There won't be `prevkeys` or `previds`
  // if it is the first page. These accumulate during paging so that it
  // is possible to go backwards.
  mod.get = function (startkey, startid, prevkeys, previds) {
    // The URL given as one of the original args.
    var url = args.url + '?';
    // This would be a custom index ID.
    var indexId = args.indexId;
    // The given limit.
    var limit = limitField().value * 1;
    // Where the next page will be displayed.
    var target = args.target;
    // The filter is used to constrain the values listed.
    var filterVal = document.getElementById(prefix + '-filter').value;
    var state = {
      sk: startkey,
      sid: startid,
      pks: prevkeys,
      pids: previds
    };

    if (!state.pks) {

      if (args.filterMod) {
        filterVal = args.filterMod(filterVal);
      }

      state.sk = escapeValue(filterVal);
      state.pks = [];
      state.pids = [];
    }

    if (state.sk) {
      url = url + 'startkey=' + window.escape(window.atob(state.sk));
      if (state.sid) {
        url = url + '&startkey_docid=' + state.sid;
      }
    }

    if (limit) {
      url = url + '&limit=' + (limit + 1);
    } else {
      limitField().value = 25;
      url = url + '&limit=26';
    }

    if (indexId) {
      url = url + '&index=' + indexId;
    }

    ajax.get(url, function (req) {
      mod.fill(req, state, target);
    });

    return mod;
  };

  mod.fill = function (req, state, target) {
    var limit = limitField().value * 1;
    var respJSON;
    var lastrow;
    var newRows;

    var prevElem = function () {
      return document.getElementById('previous-' + prefix + '-page');
    };

    var nextElem = function () {
      return document.getElementById('next-' + prefix + '-page');
    };

    var prevHandler = function () {
      mod.get(state.pks.pop(), state.pids.pop(), state.pks, state.pids);
    };

    var nextHandler = function () {
      var firstElem = document.getElementById('first-' + prefix + '-element');
      var nextkey = nextElem().getAttribute('data-startkey');
      var nextid = nextElem().getAttribute('data-startid');
      var prevkey = firstElem.getAttribute('data-first-key');
      var previd = firstElem.getAttribute('data-first-id');
      state.pks.push(prevkey);
      state.pids.push(previd);

      mod.get(nextkey, nextid, state.pks, state.pids);
    };

    if (format === undefined) {
      respJSON = req.response;
    } else {
      respJSON = format(req.response);
    }

    newRows = respJSON.rows.map(function (item, index, thisArray) {
      item.encoded_key = escapeValue(item.key);
      return item;
    });

    lastrow = newRows.slice(-1);

    if (newRows[0]) {
      newRows[0].firstrow = true;
    }

    if (newRows.length > limit) {
      respJSON.rows = newRows.slice(0, -1);
    } else {
      respJSON.rows = newRows;
      respJSON.lastpage = true;
    }

    respJSON.lastrow = lastrow;
    respJSON.prefix = prefix;

    target.innerHTML = templates['paged-listing'](respJSON, {
      'listed-element': templates.templates[prefix + '-element']
    });

    nextElem().onclick = nextHandler;
    prevElem().onclick = prevHandler;

    // Disable the previous button if we're at the beginning
    if (state.pks.length === 0) {
      prevElem().classList.add('hidden');
    }

    // Disable the next button if we're at the end
    if (nextElem().getAttribute('data-last-page')) {
      nextElem().classList.add('hidden');
    }

    return mod;
  };

  return mod;
};

exports.pager = pager;

},{"ajax":105,"templates":52}],94:[function(require,module,exports){
// # Panel Toggler
//
// Interface elements called panels can be visible or hidden.

// Given an element that points to a panel id with a `data-panel`
// attribute, toggle the panel's visibility.
var panelToggler = function (target) {
  'use strict';

  var panel;

  if ($(target).attr('data-panel')) {
    panel = $('#' + $(target).attr('data-panel'));
  } else {
    panel = $(target).closest('.panel');
  }

  if (panel.css('display') === 'none') {
    panel.css('display', 'table-cell');
  } else {
    panel.css('display', 'none');
  }

  return target;
};

exports.panelToggler = panelToggler;

},{}],95:[function(require,module,exports){
// # Path helper
//
// NOTE: This is only used by `config/doctype-tab.js` and
// `documents/fieldsets`. It may be possible to refactor and remove
// this since the `config/doctype-tab.js` code is already deprecated.
//
// *Implicit depends:* DOM
//
// This function returns an object with various helpers for URL
// path operations. In this application a common pattern in paths is
// `doctypes/<doctypeid>/fieldsets/<fiedsetid>/fields/<fieldid>`. The
// path function below will take a source, which is a jQuery object,
// such as `$('#some-id')`, which has an attribute named `data-group-id`
// having a value of the id of an element that stores data relevant to
// the current context as HTML data attributes, in particular the ids of
// doctypes, fieldsets and/or fields. The category is one of 'field',
// 'fieldset' or 'doctype'. The section argument is a section of the
// application, such as 'config' that will be prefixed to the path.
//
// #### Example HTML:
//
//     <div
//       id='someid'
//       data-fieldset-fieldset='fsid'
//       data-fieldset-doctype='did'></div>
//
//     <div
//      id='thisid'
//      data-group-id='someid'>
//
// #### Example usage:
//
//     var t = getElementById('thisid');
//     mypath = path($(t, 'fieldset');
//     mypath.toString() == 'doctypes/did/fieldsets/fsid';
//
//     var t = getElementById('thisid');
//     mypath = path(t, 'fieldset', 'config');
//     mypath.toString() == 'config/doctypes/did/fieldsets/fsid';
//
//     var t = getElementById('thisid');
//     mypath = path(t, 'fieldset');
//     mypath.fieldset = false; // unsets the fielset id
//     mypath.toString() == 'doctypes/did/fieldsets'; // all fieldsets
//
// Note that the category matches the x of `data-x` in `someid`. Different
// values may be held for doctype or field in the same element. Sometimes
// this leads to repetition of information and a better scheme may be
// forthcoming. The positive side is that information about different
// paths may be held in the same location.
//
// ### CouchDB Revision Numbers
//
// Above, a revision could have been added to someid as `data-fieldset-rev`.
//
// #### More Information
//
// For more information on how data attributes are used in this application,
// see [store.js](./store.html).
//
// ## Manipulating the object
//
// Also note that setting certain path elements to false (or undefined)
// will exclude their ids from the end result. Setting the element to a
// different id would cause the path to be altered appropriately. This
// allows one to cleanly manipulate the paths without performing string
// manipulation.
//
// ## PUT, POST and DELETE using the object
//
// There are also helpers for using the path the work with the resource
// it points to.
//
// #### Example:
//
//     mypath = path(HTMLElement, 'fieldset');
//     mypath.put(object, callback);
//     mypath.post(object, callback);
//     mypath.del(callback);
//
// Object is an Javascript object that can be encoded as JSON, callback
// will be run on success and context provides information the environment
// from which the method was called, usually `this` is supplied.
//
// The object will be sent to the path that would be returned by the
// toString method using the method implied by the above method's names.
//
// ### Error handlers
//
// Within the context of this application it is assumed that fairly
// standard things will be done with error responces so they are left
// alone.

// Variable Definitions

var store = require('store').store;
var ajax = require('ajax');

// Exported functions

// Object initialization
var path = function (source, category, section) {
  'use strict';

  var mod = {};
  var prefix;

  if (category) {
    prefix = category + '-';
  } else {
    prefix = '';
  }

  if (section) {
    mod.string = section + '/';
  } else {
    mod.string = '';
  }

  mod.category = category;
  mod.origin = source;
  mod.type = prefix + 'path';
  mod.valid_components = ['doctype', 'fieldset', 'field'];
  var s = store(mod.origin);

  mod.valid_components.forEach(function (item) {
    mod[item] = (function () {
      var value = s.get(prefix + item);
      return value;
    })();
  });

  mod.rev = s.get(prefix + 'rev');

  mod.doctype = s.get(prefix + 'doctype');

  // TODO: there is a redundant abstraction of `send` here that
  // already exists in the `ajax` module.
  mod.send = function (object, method, callback) {
    ajax.send(mod.toString(), object, method, callback);
    return mod;
  };

  mod.put = function (object, callback) {
    mod.send(object, 'PUT', callback);
    return mod;
  };

  mod.post = function (object, callback) {
    mod.send(object, 'POST', callback);
    return mod;
  };

  mod.del = function (callback) {
    mod.send({}, 'DELETE', callback);
    return mod;
  };

  mod.toString = function () {
    var rev;

    var pathString =
      mod.string.concat(
        mod.valid_components.map(

          function (item) {
            var plural = item + 's';
            var value = mod[item];
            var retval = null;

            if (value) {
              retval = plural + '/' + value;
            } else if (item === mod.category) {
              retval = plural;
            }

            return retval;
          }).filter(

          function (item) {
            return (typeof item === 'string' && !item.isBlank());
          }).join('/'));

    if (mod.rev) {
      pathString = pathString.concat('?rev=' + mod.rev);
    }

    return pathString;
  };

  return mod;
};

exports.path = path;

},{"ajax":105,"store":149}],96:[function(require,module,exports){
// # The project manager
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Interface for working with projects.

// Variable Definitions

var ajax = require('ajax');
var templates = require('templates');
var init;

// ## Internal functions

// Show a brief validation message.
var updateTips = function (t, tips) {
  'use strict';

  tips.insertAdjacentHTML('beforeend', '<span class="validation-error-message">' + t + '</span>');
  tips.classList.add('ui-state-highlight');
  setTimeout(function () {
    tips.classList.remove('ui-state-highlight', 1500);
  }, 500);

  return tips;
};

// Client side validation of string length.
// TODO: use HTML 5 validation
var checkLength = function (o, n, min, max, tips) {
  'use strict';

  if (o.value.length > max || o.value.length < min) {
    o.classList.add('ui-state-error');
    updateTips('Length of ' + n + ' must be between ' + min + ' and ' + max + '.', tips);
    return false;
  } else {
    return true;
  }
};

// Delete the project with the given ID.
var deleteProject = function (id) {
  'use strict';

  if (window.confirm('Are you sure? This is permanent.')) {
    ajax.del('/projects/' + id, init);
  }
};

// ## Exported functions

// Add a project.
var add = function () {
  'use strict';

  var projectName = document.getElementById('project-name');
  var projectDescription = document.getElementById('project-description');
  var tips = document.getElementsByClassName('validate-tips')[0];
  var validationErrors = document.getElementsByClassName('validation-error-message')[0];
  var allFields = [projectName, projectDescription];
  var checkResult;

  var dialog = $('#add-dialog').dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      'Add project': function () {
        Array.prototype.forEach.call(allFields, function (item) {
          item.classList.remove('ui-state-error');
        });

        if (validationErrors) {
          validationErrors.parentNode.removeChild(validationErrors);
        }

        checkResult = checkLength(projectName, 'project name', 1, 50, tips);

        if (checkResult) {
          var data = {
            name: projectName.value,
            description: projectDescription.value
          };

          ajax.post('projects/index', data, init);

          $(this).dialog('close');
        }
      },
      'Cancel': function () {
        $(this).dialog('close');
      }
    },
    close: function () {
      Array.prototype.forEach.call(allFields, function (item) {
        item.value = '';
        item.classList.remove('ui-state-error');
      });
    }
  });

  return dialog;
};

// Add a project.
var del = function (target) {
  'use strict';

  var id = target.getAttribute('id');

  deleteProject(id);

  return true;
};

// Initialize the interface.
init = function () {
  'use strict';

  var url = '/projects/index';

  ajax.get(url, function (req) {
    var rendering = templates['project-listing'](req.response);

    document.getElementsByTagName('tbody')[0].innerHTML = rendering;
  });
};

exports.add = add;
exports.del = del;
exports.init = init;

},{"ajax":105,"templates":52}],97:[function(require,module,exports){
// # Take actions depending on reported state.
//
// This is essentially an experiment in attempting to perform actions
// based on the state of the application. It is an idea that I'm still
// working on but the idea is to avoid having functions directly call
// other functions to initiate new actions but to instead simply report
// their state and have some central authority decide what to do next.
//
// The idea is to make something like this into a worker to achieve
// concurrency.

// Variable Definitions

var commands = require('documents/commands');
var documents = require('documents/documents');
var dinfo = require('documents/information');
var editui = require('documents/editui');
var searchui = require('documents/searchui');
var setsui = require('documents/setsui');
var worksheetui = require('documents/worksheetui');
var ceditui = require('config/editui');
var cdoctypeui = require('config/doctypeui');

// Exported functions

// This is called by functions when the actions they have performed
// result in a paticular state.
var sender = function (message, arg) {
  'use strict';

  var retval;

  switch (message) {
  case 'document-init-stage-1':
    retval = dinfo.checkState();
    break;
  case 'bad-session-state':
    retval = dinfo.clearSession();
    break;
  case 'doctype-info-ready':
    retval = dinfo.makeFieldsetLookup();
    break;
  case 'fieldset-lookup-ready':
    retval = dinfo.makeLabels();
    break;
  case 'doctype-cached-info-ready':
    documents.init2();
    retval = searchui.loadSearchVals();
    worksheetui.buildTemplate();
    break;
  case 'new-set-form-submit':
    retval = setsui.saveSelected();
    break;
  case 'sets-changed':
    retval = setsui.updateSelection();
    break;
  case 'sets-form-submit':
    retval = setsui.performOp();
    break;
  case 'session-cleared':
    dinfo.setVersion();
    retval = dinfo.loadDoctype();
    break;
  case 'worksheet-form-submit':
    retval = worksheetui.fillWorksheet();
    break;
  case 'initiated-command':
    retval = commands.dialogOpen(arg);
    break;
  case 'executed-command':
    retval = commands.dialogClose();
    break;
  case 'submitted-command':
    retval = commands.execute(arg);
    break;
  case 'lost-focus':
    retval = editui.selectInput();
    break;
    // Config messages
  case 'doctypes-add':
    retval = cdoctypeui.addDoctype();
    break;
  case 'new-doctype-built':
    retval = ceditui.init(arg);
    break;
  case 'edit-doctype-requested':
    retval = ceditui.get(arg);
    break;
  case 'config-save':
    retval = ceditui.update();
    break;
  case 'config-create':
    retval = ceditui.create();
    break;
  case 'config-doctype-created':
  case 'config-doctype-deleted':
    ceditui.fresh();
    /* falls through */
  case 'config-doctype-updated':
    retval = cdoctypeui.init();
    break;
  case 'config-delete':
    retval = ceditui.remove();
    break;
  case 'config-move-up':
    retval = ceditui.elementUp();
    break;
  case 'config-move-down':
    retval = ceditui.elementDown();
    break;
  case 'config-remove-element':
    retval = ceditui.elementDelete();
    break;
  case 'config-add-text':
    retval = ceditui.addTextElement();
    break;
  case 'config-add-array':
    retval = ceditui.addArrayElement();
    break;
  case 'config-add-object':
    retval = ceditui.addObjectElement();
    break;
  case 'config-add-child-text':
    retval = ceditui.addChildTextElement();
    break;
  case 'config-add-child-array':
    retval = ceditui.addChildArrayElement();
    break;
  case 'config-add-child-object':
    retval = ceditui.addChildObjectElement();
    break;
  case 'config-clear-form':
    retval = ceditui.init();
    break;
  case 'config-copy':
    retval = ceditui.copy();
    break;
  case 'config-cut':
    retval = ceditui.cut();
    break;
  case 'config-paste':
    retval = ceditui.paste();
    break;
  case 'config-paste-child':
    retval = ceditui.pasteChild();
    break;
  case 'config-promote':
    retval = ceditui.promote();
    break;
  case 'config-demote':
    retval = ceditui.demote();
    break;
  case 'config-mark-line':
    retval = ceditui.markLine(arg);
    break;
  }

  return retval;
};

exports.sender = sender;

},{"config/doctypeui":110,"config/editui":111,"documents/commands":116,"documents/documents":117,"documents/editui":118,"documents/information":121,"documents/searchui":122,"documents/setsui":123,"documents/worksheetui":126}],98:[function(require,module,exports){
// # Session storage helpers
//
// *Implicit depends:* DOM
//
// This is primarily used to store and retrieve items with a structure
// similar to a CouchDB document.

// Exported functions

// Like put but will overwrite the previous item.
var replace = function (doc) {
  'use strict';

  window.sessionStorage[doc._id] = JSON.stringify(doc);

  return doc._id;
};

// If the item is not already in the session storage, convert it to JSON
// and store it by `_id`. Return the `_id` of the document.
var put = function (doc) {
  'use strict';

  if (!window.sessionStorage[doc._id]) {
    replace(doc);
  }

  return doc._id;
};

// Retrieve the document, which is stored as JSON, by its `_id` and
// return the parsed item. If the item does not exist, return `null`.
var get = function (docId) {
  'use strict';

  var doc = window.sessionStorage[docId];

  if (doc) {
    return JSON.parse(doc);
  } else {
    return null;
  }
};

exports.replace = replace;
exports.put = put;
exports.get = get;

},{}],99:[function(require,module,exports){
// # Set operations
//
// The 'set' is a one dimensional Array by default but by replacing the
// `member` function, other types of Arrays may be used.

// Exported functions

// Determine membership of item in the set.
var member = function (arr, x) {
  'use strict';

  var memb = arr.some(function (y) {
    return x === y;
  });
  return memb;
};

// Rebuild the array so that all values are unique. This is kind of a
// 'clean up' function used to work around the differences between arrays
// and sets.
var unique = function (x, mem) {
  'use strict';

  if (!mem) {
    mem = member;
  }
  var uniq = x.reduce(function (acc, curr) {
    if (mem(acc, curr)) {
      return acc;
    } else {
      return acc.concat([curr]);
    }
  }, []);
  return uniq;
};

// Return the union of two sets.
var union = function (xs, ys, mem) {
  'use strict';

  if (!mem) {
    mem = member;
  }
  var uni = unique(xs.concat(ys), mem);
  return uni;
};

// Return the intersection of two sets.
var intersection = function (xs, ys, mem) {
  'use strict';

  if (!mem) {
    mem = member;
  }
  var inter = xs.filter(function (x) {
    return mem(ys, x);
  });
  return inter;
};

// Return the relative complement of two sets.
var relativeComplement = function (xs, ys, mem) {
  'use strict';

  if (!mem) {
    mem = member;
  }
  var comp = xs.filter(function (x) {
    return !mem(ys, x);
  });
  return comp;
};

// Return the symmetric difference of two sets.
var symmetricDifference = function (xs, ys, mem) {
  'use strict';

  if (!mem) {
    mem = member;
  }
  var comp1 = relativeComplement(xs, ys, mem);
  var comp2 = relativeComplement(ys, xs, mem);
  var uni = union(comp1, comp2, mem);
  return uni;
};

exports.member = member;
exports.unique = unique;
exports.union = union;
exports.intersection = intersection;
exports.relativeComplement = relativeComplement;
exports.symmetricDifference = symmetricDifference;

},{}],100:[function(require,module,exports){
// # Data Attribute Storage and Retrieval Helpers
//
// *Implicit depends:* DOM
//
// It is likely that this mechanism will be replaced with a superior
// mechanism for storing data on the client about documents.

// ## Variables

var utils = require('utils');
var r = require('lib/recurse');

// ## Internal functions

// ## External functions

// Takes an Element and returns an object with helper methods for
// getting and putting custom data attribute values.
var store = function (elem) {
  'use strict';

  var mod = {};

  // This funtion takes a key that corresponds to the name of the data
  // attribute without the `data-` prefix. The element is expected to have
  // an attribute data-group-id with a value that is the id of the
  // element actually holding the data.
  //
  // ### Examples
  //
  // Given the following HTML:
  //
  //     <div
  //       id='someid'
  //       data-fieldset-fieldset='fsid'
  //       data-fieldset-doctype='did'></div>
  //
  //     <div
  //      id='thisid'
  //      data-group-id='someid'>
  //
  // The `data-fieldset-doctype` may be retrieved like this:
  //
  //     var thisId = document.getElementById('thisid');
  //     store(thisId).get('fieldset-doctype') == 'did';
  //
  // This HTML contains a level of indirection and demonstrates the use
  // of the `data-group-id`:
  //
  //     <div
  //       id='someid2'
  //       data-fieldset-fieldset='fsid'
  //       data-fieldset-doctype='did'></div>
  //
  //     <div
  //       id='someid'
  //       data-group-id='someid2'
  //       data-fieldset-fieldset='fsid'></div>
  //
  //     <div
  //      id='thisid'
  //      data-group-id='someid'></div>
  //
  // The `data-fieldset-doctype` may be retrieved like this:
  //
  //     var thisId = document.getElementById('thisid');
  //     store(thisId).get('fieldset-doctype') == 'did';
  //
  mod.get = function (key) {
    var keycc = key.cc();
    var prelim = elem.dataset[keycc];

    if (prelim) {
      return prelim;
    }

    var getValue1 = function (key, elem, id) {
      var gid = elem.dataset.groupId;
      var store = document.getElementById(gid);
      var val = store.dataset[key];
      var next = store.dataset.groupId;

      if (val === undefined && next !== undefined && gid !== next) {
        return getValue1.r(key, store, id);
      }

      return id.r(val);
    };

    return getValue1.t(keycc, elem, r.identity);
  };

  // Like 'get' but will decode base64 encoded values.
  mod.get64 = function (key) {
    var retval = mod.get(key);
    retval = utils.Base64.decode(retval.replace(/'/g, '')).replace(/(^'|'$)/g, '');
    return retval;
  };

  //  This function will set an attribute at the target with a name
  //  corresponding to key and a value of value.
  mod.put = function (key, value) {
    var keycc = key.cc();
    var dataElem = elem.dataset.groupId;
    document.getElementById(dataElem).dataset[keycc] = value;
  };

  //  Helper function for attributes that begin with `data-fieldset`.
  mod.fs = function (key) {
    return mod.get('fieldset-' + key);
  };

  //  Helper function for attributes that begin with `data-field`.
  mod.f = function (key) {
    return mod.get('field-' + key);
  };

  //  Helper function for attributes that begin with `data-document`.
  mod.d = function (key) {
    return mod.get('document-' + key);
  };

  return mod;
};

exports.store = store;

},{"lib/recurse":153,"utils":150}],101:[function(require,module,exports){
// # Misc

// Exported functions

// safer(ish) string to number. The difference is that in this app
// I am using '' if the string isn't a valid number.
var stringToNumber = function (string) {
  'use strict';

  if (typeof string === 'string' && !isNaN(string) && string !== '') {
    return string * 1;
  } else {
    return '';
  }
};

// A predicate function to detect blankness of various object types.
var isBlank = function (value) {
  'use strict';

  return (((/^\s*$/).test(value)) || (value === null) || (value === undefined) || (typeof value === 'number' && isNaN(value)) || (Object.prototype.toString.call(value) === '[object Array]' && value.length === 0));
};

// A predicate to test if the input is a string containing 32 characters
// limited to hexidecimal digits.
var validID = function (id) {
  'use strict';

  return !!id.match(/^[a-f0-9]{32}$/);
};

// Base64 encode / decode
// Taken from <http://www.webtoolkit.info/>
var Base64 = {
  // private property
  _keyStr: 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=',

  // public method for encoding
  encode: function (input) {
    'use strict';

    var output = '';
    var chr1, chr2, chr3, enc1, enc2, enc3, enc4;
    var i = 0;

    input = Base64._utf8_encode(input);

    while (i < input.length) {

      chr1 = input.charCodeAt(i++);
      chr2 = input.charCodeAt(i++);
      chr3 = input.charCodeAt(i++);

      enc1 = chr1 >> 2;
      enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
      enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
      enc4 = chr3 & 63;

      if (isNaN(chr2)) {
        enc3 = enc4 = 64;
      } else if (isNaN(chr3)) {
        enc4 = 64;
      }

      output = output + this._keyStr.charAt(enc1) + this._keyStr.charAt(enc2) + this._keyStr.charAt(enc3) + this._keyStr.charAt(enc4);

    }

    return output;
  },

  // public method for decoding
  decode: function (input) {
    'use strict';

    var output = '';
    var chr1, chr2, chr3;
    var enc1, enc2, enc3, enc4;
    var i = 0;

    input = input.replace(/[^A-Za-z0-9\+\/\=]/g, '');

    while (i < input.length) {

      enc1 = this._keyStr.indexOf(input.charAt(i++));
      enc2 = this._keyStr.indexOf(input.charAt(i++));
      enc3 = this._keyStr.indexOf(input.charAt(i++));
      enc4 = this._keyStr.indexOf(input.charAt(i++));

      chr1 = (enc1 << 2) | (enc2 >> 4);
      chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
      chr3 = ((enc3 & 3) << 6) | enc4;

      output = output + String.fromCharCode(chr1);

      if (enc3 !== 64) {
        output = output + String.fromCharCode(chr2);
      }
      if (enc4 !== 64) {
        output = output + String.fromCharCode(chr3);
      }

    }

    output = Base64._utf8_decode(output);

    return output;

  },

  // private method for UTF-8 encoding
  _utf8_encode: function (string) {
    'use strict';

    string = string.replace(/\r\n/g, '\n');
    var utftext = '';

    for (var n = 0; n < string.length; n++) {

      var c = string.charCodeAt(n);

      if (c < 128) {
        utftext += String.fromCharCode(c);
      } else if ((c > 127) && (c < 2048)) {
        utftext += String.fromCharCode((c >> 6) | 192);
        utftext += String.fromCharCode((c & 63) | 128);
      } else {
        utftext += String.fromCharCode((c >> 12) | 224);
        utftext += String.fromCharCode(((c >> 6) & 63) | 128);
        utftext += String.fromCharCode((c & 63) | 128);
      }

    }

    return utftext;
  },

  // private method for UTF-8 decoding
  _utf8_decode: function (utftext) {
    'use strict';

    var string = '';
    var i = 0;
    var c = 0;
    var c1 = 0;
    var c2 = 0;
    var c3 = 0;

    while (i < utftext.length) {

      c = utftext.charCodeAt(i);

      if (c < 128) {
        string += String.fromCharCode(c);
        i++;
      } else if ((c > 191) && (c < 224)) {
        c2 = utftext.charCodeAt(i + 1);
        string += String.fromCharCode(((c & 31) << 6) | (c2 & 63));
        i += 2;
      } else {
        c2 = utftext.charCodeAt(i + 1);
        c3 = utftext.charCodeAt(i + 2);
        string += String.fromCharCode(((c & 15) << 12) | ((c2 & 63) << 6) | (c3 & 63));
        i += 3;
      }

    }

    return string;
  }

};

exports.stringToNumber = stringToNumber;
exports.isBlank = isBlank;
exports.validID = validID;
exports.Base64 = Base64;

},{}],"templates.js":[function(require,module,exports){
module.exports=require('mkFiG5');
},{}],"mkFiG5":[function(require,module,exports){
module.exports=require(52)
},{"hogan.js":24}],104:[function(require,module,exports){
module.exports=require(52)
},{"hogan.js":24}],105:[function(require,module,exports){
module.exports=require(53)
},{"flash":128}],106:[function(require,module,exports){
module.exports=require(55)
},{"documents/searchui":122,"index_tool/new-dialog":139}],107:[function(require,module,exports){
module.exports=require(56)
},{"config/maintenanceui":112,"dispatcher":114,"documents/editui":118,"documents/fieldsets":119,"documents/indexui":120,"documents/searchui":122,"documents/setsui":123,"documents/viewui":125,"documents/worksheetui":126,"file_manager/fm":127,"form":129,"index_tool/ieditui":134,"panel-toggle":144,"projects/projectui":145,"sender":146}],108:[function(require,module,exports){
module.exports=require(57)
},{"pager":143,"templates":104}],109:[function(require,module,exports){
module.exports=require(58)
},{"config/charsequi":108,"config/doctypeui":110,"config/editui":111,"config/maintenanceui":112}],110:[function(require,module,exports){
module.exports=require(59)
},{"../sender.js":97,"node-uuid":51,"pager":143,"templates":104}],111:[function(require,module,exports){
module.exports=require(60)
},{"../sender.js":97,"ajax":105,"formalize":130,"sess":147}],112:[function(require,module,exports){
module.exports=require(61)
},{"ajax":105,"flash":128,"templates":104}],113:[function(require,module,exports){
module.exports=require(62)
},{"config/editui":111,"dispatcher":114,"documents/searchui":122,"documents/worksheetui":126,"panel-toggle":144}],114:[function(require,module,exports){
module.exports=require(63)
},{}],115:[function(require,module,exports){
module.exports=require(64)
},{"documents/information":121,"pager":143}],116:[function(require,module,exports){
module.exports=require(65)
},{"../sender.js":97,"documents/editui":118}],117:[function(require,module,exports){
module.exports=require(66)
},{"../sender.js":97,"./editui.js":67,"ajax":105,"documents/changeui":115,"documents/indexui":120,"documents/information":121,"documents/setsui":123,"documents/ui-shared":124,"documents/viewui":125}],118:[function(require,module,exports){
module.exports=require(67)
},{"./fieldsets.js":68,"ajax":105,"documents/indexui":120,"documents/information":121,"documents/ui-shared":124,"documents/viewui":125,"flash":128,"form":129,"node-uuid":51,"store":149,"templates":104}],119:[function(require,module,exports){
module.exports=require(68)
},{"../path.js":95,"./editui.js":67,"ajax":105,"documents/information":121,"documents/ui-shared":124,"store":149,"templates":104,"utils":150}],120:[function(require,module,exports){
module.exports=require(69)
},{"./editui.js":67,"./viewui.js":74,"ajax":105,"documents/ui-shared":124,"pager":143,"templates":104}],121:[function(require,module,exports){
module.exports=require(70)
},{"../sender.js":97,"ajax":105,"documents/ui-shared":124}],122:[function(require,module,exports){
module.exports=require(71)
},{"ajax":105,"documents/information":121,"documents/setsui":123,"documents/ui-shared":124,"sets":148,"templates":104,"utils":150}],123:[function(require,module,exports){
module.exports=require(72)
},{"../sender.js":97,"documents/information":121,"flash":128,"sets":148,"templates":104,"utils":150}],124:[function(require,module,exports){
module.exports=require(73)
},{"form":129}],125:[function(require,module,exports){
module.exports=require(74)
},{"./editui.js":67,"./fieldsets.js":68,"ajax":105,"documents/indexui":120,"documents/ui-shared":124,"flash":128,"store":149,"templates":104}],126:[function(require,module,exports){
module.exports=require(75)
},{"ajax":105,"documents/information":121,"documents/setsui":123,"flash":128,"hogan.js":24,"templates":104}],127:[function(require,module,exports){
module.exports=require(76)
},{"ajax":105,"flash":128}],128:[function(require,module,exports){
module.exports=require(77)
},{}],129:[function(require,module,exports){
module.exports=require(78)
},{"ajax":105}],130:[function(require,module,exports){
module.exports=require(79)
},{"formalize_from":131,"formalize_to":132}],131:[function(require,module,exports){
module.exports=require(80)
},{"htmlparser2":37}],132:[function(require,module,exports){
module.exports=require(81)
},{"lib/json_to":152,"node-uuid":51}],133:[function(require,module,exports){
module.exports=require(83)
},{"ajax":105,"form":129,"index_tool/ievents":135,"index_tool/ihelpers":136}],134:[function(require,module,exports){
module.exports=require(84)
},{"ajax":105,"flash":128,"index_tool/builder-dialog":133,"index_tool/ihelpers":136,"index_tool/ilistingui":137,"index_tool/ipreviewui":138,"index_tool/new-dialog":139,"index_tool/replace-dialog":140}],135:[function(require,module,exports){
module.exports=require(85)
},{"index_tool/ihelpers":136}],136:[function(require,module,exports){
module.exports=require(86)
},{"ajax":105,"form":129,"sess":147}],137:[function(require,module,exports){
module.exports=require(87)
},{"ajax":105,"templates":104}],138:[function(require,module,exports){
module.exports=require(88)
},{"pager":143}],139:[function(require,module,exports){
module.exports=require(89)
},{"ajax":105,"form":129,"index_tool/ihelpers":136,"index_tool/ilistingui":137}],140:[function(require,module,exports){
module.exports=require(90)
},{"index_tool/ihelpers":136}],141:[function(require,module,exports){
module.exports=require(91)
},{}],142:[function(require,module,exports){
module.exports=require(92)
},{"config/charsequi":108,"config/doctypeui":110,"documents/changeui":115,"documents/editui":118,"documents/indexui":120,"documents/searchui":122,"documents/viewui":125,"index_tool/ipreviewui":138,"jquery.hotkeys":141,"sender":146}],143:[function(require,module,exports){
module.exports=require(93)
},{"ajax":105,"templates":104}],144:[function(require,module,exports){
module.exports=require(94)
},{}],145:[function(require,module,exports){
module.exports=require(96)
},{"ajax":105,"templates":104}],146:[function(require,module,exports){
module.exports=require(97)
},{"config/doctypeui":110,"config/editui":111,"documents/commands":116,"documents/documents":117,"documents/editui":118,"documents/information":121,"documents/searchui":122,"documents/setsui":123,"documents/worksheetui":126}],147:[function(require,module,exports){
module.exports=require(98)
},{}],148:[function(require,module,exports){
module.exports=require(99)
},{}],149:[function(require,module,exports){
module.exports=require(100)
},{"lib/recurse":153,"utils":150}],150:[function(require,module,exports){
module.exports=require(101)
},{}],151:[function(require,module,exports){
// # Json Parse
//
// *implicit dependencies:* JSON
//
// Convert a subset of JSON to an abstract syntax.

// ## Variable Definitions

var r = require('lib/recurse');

// ## Internal Functions

// Get the 'type', which may not correspond to the JavaScript type.
var getType = function (val) {
  'use strict';

  var retval;

  if (typeof val === 'string') {
    return 'string';
  } else if (typeof val === 'boolean') {
    return 'boolean';
  } else if (typeof val === 'number') {
    return 'number';
  } else if (val === null) {
    return 'null';
  } else if (val instanceof Array) {
    return 'array';
  } else if (val instanceof Object) {
    return 'object';
  }
};

// Process key value pairs in an object and return an object that
// describes the original object.
var getKeyVals = function (o) {
  'use strict';

  return Object.keys(o).map(function (k) {
    var val = o[k];

    return {
      key: (o instanceof Array) ? false : k,
      index: (o instanceof Array) ? k * 1 : false,
      type: getType(val),
      value: val
    };
  });
};

// Transform the object into an AST that should be easier to work with
// in templating systems, etc.
var transform = function (obj) {
  'use strict';

  var start = {
    root: []
  };

  var transform_ = function (o, rest, accObj, id) {
    var result;
    var keyVals = getKeyVals(o.object);

    result = keyVals.reduce(function (acc, x) {
      if (x.type === 'array' || x.type === 'object') {
        return acc.concat({
          object: x.value,
          key: 'value',
          parent: x
        });
      } else {
        return acc;
      }
    }, []);

    rest = rest.concat(result);
    o.parent[o.key] = keyVals;

    if (rest && rest.length !== 0) {
      return transform_.r(rest[0], rest.slice(1), accObj, id);
    } else {
      return id.r(accObj);
    }
  };

  if (obj === null) {
    // Note that this may seem like an odd choice but it is useful to
    // have as a return value based on the assumptions of the software
    // this module relies on.
    return {};
  } else {
    return transform_.t({
      object: obj,
      parent: start,
      key: 'root'
    }, [], start, r.identity);
  }
};

// Simply a call to JSON.parse with some special error handling.
var tryParseJSON = function (jsn) {
  'use strict';

  var obj;

  try {
    obj = JSON.parse(jsn);
  } catch (e) {
    switch (e.name) {
    case 'SyntaxError':
      e.message = 'invalid JSON: ' + JSON.stringify(jsn);
      throw e;
    default:
      throw e;
    }
  }

  // I've tested this and strangely enough JSON.parse(null) === null
  if (jsn === null) {
    throw new SyntaxError('invalid JSON: null');
  }

  return obj;
};

// Some types of valid JSON are not useful in this context..
var validate = function (obj) {
  'use strict';

  var msg = 'cannot build AST from: ';

  if (typeof obj === 'string') {
    throw msg + 'string';
  } else if (typeof obj === 'number') {
    throw msg + 'number';
  } else if (obj !== null && obj.constructor === Array) {
    throw msg + 'array';
  }

  return obj;
};

// ## External Functions

var parse = function (jsn) {
  'use strict';

  var obj = tryParseJSON(jsn);

  obj = validate(obj);

  return transform(obj);
};

exports.parse = parse;

},{"lib/recurse":153}],152:[function(require,module,exports){
// # JSON to
//
// Convert JSON to something else.

var r = require('lib/recurse');
var json_parse = require('lib/json_parse');

// ## Internal Functions

// Insert the strings in the acc object.
var insert = function (lr, acc) {
  'use strict';

  acc.left = acc.left + lr[0];
  acc.right = lr[1] + acc.right;

  return acc;
};

// Process an item. This is where `complex` and `simple` supplied
// functions are called.
var process = function (item, acc, funs, options) {
  'use strict';

  if (item && item.value !== undefined) {
    if (item.type !== 'array' && item.type !== 'object') {
      acc = insert(funs.simple(item, options), acc);
    } else if (item.type && (item.type === 'object' || item.type === 'array')) {
      acc = insert(funs.complex(item, options), acc);
    }
  }

  return acc;
};

// Pop the accstack and insert the current acc left and right on the
// left of the poped object.
var accInsert = function (accstack, acc) {
  'use strict';

  var acc2 = accstack.pop();
  acc2.left = acc2.left + acc.left + acc.right;

  return acc2;
};

// The produce an output based on the AST, functions and options.
var to = function (ast, funs, options) {
  'use strict';

  var acc = insert(funs.context(options), {
    left: '',
    right: ''
  });
  var result;

  // This will be used to recurse the AST. The `head` variable is the
  // current portion of the AST to process. The `rest` variable is the
  // remaining portion. The `acc` is an accumulator that stores the right
  // and left ends of a string that is built up during processing. `stack`
  // is used to save state while descending complex objects. `accstack`
  // is used to store `acc` context when descending. `id` is a reference
  // to the identity function to define the base case.
  var _to = function (head, rest, acc, stack, accstack, id) {
    var isNotObject = head && (head.type !== 'array' && head.type !== 'object');
    var done = rest.length === 0;
    var depleted = stack.length === 0;
    var next;
    var acc2;

    // There are no more items, the stack is depleted and the value
    // doesn't need to be descended, so return the accumulator.
    if (!head && done && depleted) {
      return id.r(acc);
    }
    // If there is more on the stack, process it. We'll want to ignore
    // objects so that they can be handled in the else clause. We'll also
    // want to continue if `head` is undefined, which will indicate that
    // we've hit the end of object or array values.
    else if (!depleted && done && (isNotObject || !head)) {
      // Pop the next group of stored information off the stack where
      // it was previously stored by the 'else' clause below.
      next = stack.pop();

      // This will change the acc depending on `head` information.
      acc = process(head, acc, funs, options);

      // This will nest the current acc string values inside the
      // parent.
      acc2 = accInsert(accstack, acc);

      // Use next instead of `head` and `rest`, `rest` was already
      // depleted in this step.
      return _to.r(next[0], next.slice(1), acc2, stack, accstack, id);
    }
    // Unless it is a complex value (such as array or object), process
    // the value and move on to the next field.
    else if (isNotObject) {
      acc = process(head, acc, funs, options);

      return _to.r(rest[0], rest.slice(1), acc, stack, accstack, id);
    }
    // Otherwise descend the complex value.
    else {
      acc2 = {
        left: '',
        right: ''
      };

      // Push the remaining values onto the stack so they will be
      // processed later.
      stack.push(rest);
      // Push the accumulated left and right strings on to the
      // stack. This will allow them to be pulled out to nest acc2
      // later on.
      accstack.push(acc);

      acc = process(head, acc2, funs, options);

      // Now use the values to specify `head` and `rest`.
      return _to.r(head.value[0], head.value.slice(1), acc2, stack, accstack, id);
    }
  };

  // When the original object isn't null and there is a root.
  if (ast.not_null && ast.root) {
    acc = insert(funs.root(options), acc);

    // If there is more than just an empty list of fields, begin
    // recursion.
    if (ast.root && ast.root.length > 0) {
      result = _to.t(ast.root[0], ast.root.slice(1), acc, [], [], r.identity);
    }
  }

  return acc.left + acc.right;
};

// ## External Functions

// Given a valid subset of JSON (see `json_parse.js`), transform the JSON.
//
// The transformation is determined by functions supplied in the second
// argument. The following must be supplied:
//
// * context * root * simple * complex
//
// All of the above take the `options` argument supplied to this function
// as their final arguement.
//
// `context` will be part of the output even if the JSON provided is
// `null`. It takes only the default argument.
//
// `root` wraps the root of the JSON object. It takes only the default
// argument.
//
// `simple` is for values that are not arrays or objects. It takes the
// current item as an argument. (See `json_parse.js` for an example of
// the form of the item object.)
//
// `complex` wraps objects and arrays. It takes the current item as
// an argument.
//
// The content of options is specified by the caller. It is passed to
// the supplied functions.
var transform = function (json, funs, options) {
  'use strict';

  options = options ? options : {};
  var ast = json_parse.parse(json);

  ast.not_null = json !== 'null';

  return to(ast, funs, options);
};

exports.transform = transform;

},{"lib/json_parse":151,"lib/recurse":153}],153:[function(require,module,exports){
// # Recursion
//
// Tail call optimization taken from Spencer Tipping's Javascript in Ten
// Minutes.
//
// For more information see:
// <https://github.com/spencertipping/js-in-ten-minutes>

// ## Exported Functions

// Identity function
var identity = function (x) {
  'use strict';

  return x;
};

// Adds the prototype functions
(function () {
  'use strict';

  // Return the values to apply
  Function.prototype.r = function () {
    return [this, arguments];
  };

  // Tail call function
  Function.prototype.t = function () {
    var c = [this, arguments];
    var escape = arguments[arguments.length - 1];
    while (c[0] !== escape) {
      c = c[0].apply(this, c[1]);
    }
    return escape.apply(this, c[1]);
  };

  return true;
})();

exports.identity = identity;

},{}]},{},[53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101])