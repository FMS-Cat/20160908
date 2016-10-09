(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({"/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160908/src/script/glcat.js":[function(require,module,exports){
'use strict';

Object.defineProperty(exports, "__esModule", {
	value: true
});

var _createClass = function () {
	function defineProperties(target, props) {
		for (var i = 0; i < props.length; i++) {
			var descriptor = props[i];descriptor.enumerable = descriptor.enumerable || false;descriptor.configurable = true;if ("value" in descriptor) descriptor.writable = true;Object.defineProperty(target, descriptor.key, descriptor);
		}
	}return function (Constructor, protoProps, staticProps) {
		if (protoProps) defineProperties(Constructor.prototype, protoProps);if (staticProps) defineProperties(Constructor, staticProps);return Constructor;
	};
}();

function _classCallCheck(instance, Constructor) {
	if (!(instance instanceof Constructor)) {
		throw new TypeError("Cannot call a class as a function");
	}
}

var GLCat = function () {
	function GLCat(_gl) {
		_classCallCheck(this, GLCat);

		this.gl = _gl;
		var it = this;
		var gl = it.gl;

		gl.enable(gl.DEPTH_TEST);
		gl.depthFunc(gl.LEQUAL);
		gl.enable(gl.BLEND);
		gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

		gl.getExtension('OES_texture_float');
		gl.getExtension('OES_float_linear');
		gl.getExtension('OES_half_float_linear');

		it.program = null;
	}

	_createClass(GLCat, [{
		key: 'createProgram',
		value: function createProgram(_vert, _frag, _onError) {

			var it = this;
			var gl = it.gl;

			var error = void 0;
			if (typeof _onError === 'function') {
				error = _onError;
			} else {
				error = function error(_str) {
					console.error(_str);
				};
			}

			var vert = gl.createShader(gl.VERTEX_SHADER);
			gl.shaderSource(vert, _vert);
			gl.compileShader(vert);
			if (!gl.getShaderParameter(vert, gl.COMPILE_STATUS)) {
				error(gl.getShaderInfoLog(vert));
				return null;
			}

			var frag = gl.createShader(gl.FRAGMENT_SHADER);
			gl.shaderSource(frag, _frag);
			gl.compileShader(frag);
			if (!gl.getShaderParameter(frag, gl.COMPILE_STATUS)) {
				error(gl.getShaderInfoLog(frag));
				return null;
			}

			var program = gl.createProgram();
			gl.attachShader(program, vert);
			gl.attachShader(program, frag);
			gl.linkProgram(program);
			if (gl.getProgramParameter(program, gl.LINK_STATUS)) {
				program.locations = {};
				return program;
			} else {
				error(gl.getProgramInfoLog(program));
				return null;
			}
		}
	}, {
		key: 'useProgram',
		value: function useProgram(_program) {

			var it = this;
			var gl = it.gl;

			gl.useProgram(_program);
			it.program = _program;
		}
	}, {
		key: 'createVertexbuffer',
		value: function createVertexbuffer(_array) {

			var it = this;
			var gl = it.gl;

			var buffer = gl.createBuffer();

			gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
			gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(_array), gl.STATIC_DRAW);
			gl.bindBuffer(gl.ARRAY_BUFFER, null);

			buffer.length = _array.length;
			return buffer;
		}
	}, {
		key: 'createIndexbuffer',
		value: function createIndexbuffer(_array) {

			var it = this;
			var gl = it.gl;

			var buffer = gl.createBuffer();

			gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, buffer);
			gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Int16Array(_array), gl.STATIC_DRAW);
			gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, null);

			buffer.length = _array.length;
			return buffer;
		}
	}, {
		key: 'attribute',
		value: function attribute(_name, _buffer, _stride) {

			var it = this;
			var gl = it.gl;

			var location = void 0;
			if (it.program.locations[_name]) {
				location = it.program.locations[_name];
			} else {
				location = gl.getAttribLocation(it.program, _name);
				it.program.locations[_name] = location;
			}

			gl.bindBuffer(gl.ARRAY_BUFFER, _buffer);
			gl.enableVertexAttribArray(location);
			gl.vertexAttribPointer(location, _stride, gl.FLOAT, false, 0, 0);

			gl.bindBuffer(gl.ARRAY_BUFFER, null);
		}
	}, {
		key: 'getUniformLocation',
		value: function getUniformLocation(_name) {

			var it = this;
			var gl = it.gl;

			var location = void 0;

			if (it.program.locations[_name]) {
				location = it.program.locations[_name];
			} else {
				location = gl.getUniformLocation(it.program, _name);
				it.program.locations[_name] = location;
			}

			return location;
		}
	}, {
		key: 'uniform1i',
		value: function uniform1i(_name, _value) {

			var it = this;
			var gl = it.gl;

			var location = it.getUniformLocation(_name);
			gl.uniform1i(location, _value);
		}
	}, {
		key: 'uniform1f',
		value: function uniform1f(_name, _value) {

			var it = this;
			var gl = it.gl;

			var location = it.getUniformLocation(_name);
			gl.uniform1f(location, _value);
		}
	}, {
		key: 'uniform2fv',
		value: function uniform2fv(_name, _value) {

			var it = this;
			var gl = it.gl;

			var location = it.getUniformLocation(_name);
			gl.uniform2fv(location, _value);
		}
	}, {
		key: 'uniform3fv',
		value: function uniform3fv(_name, _value) {

			var it = this;
			var gl = it.gl;

			var location = it.getUniformLocation(_name);
			gl.uniform3fv(location, _value);
		}
	}, {
		key: 'uniformCubemap',
		value: function uniformCubemap(_name, _texture, _number) {

			var it = this;
			var gl = it.gl;

			var location = it.getUniformLocation(_name);
			gl.activeTexture(gl.TEXTURE0 + _number);
			gl.bindTexture(gl.TEXTURE_CUBE_MAP, _texture);
			gl.uniform1i(location, _number);
		}
	}, {
		key: 'uniformTexture',
		value: function uniformTexture(_name, _texture, _number) {

			var it = this;
			var gl = it.gl;

			var location = it.getUniformLocation(_name);
			gl.activeTexture(gl.TEXTURE0 + _number);
			gl.bindTexture(gl.TEXTURE_2D, _texture);
			gl.uniform1i(location, _number);
		}
	}, {
		key: 'createTexture',
		value: function createTexture() {

			var it = this;
			var gl = it.gl;

			var texture = gl.createTexture();
			gl.bindTexture(gl.TEXTURE_2D, texture);
			gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
			gl.bindTexture(gl.TEXTURE_2D, null);

			return texture;
		}
	}, {
		key: 'textureFilter',
		value: function textureFilter(_texture, _filter) {

			var it = this;
			var gl = it.gl;

			gl.bindTexture(gl.TEXTURE_2D, _texture);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, _filter);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, _filter);
			gl.bindTexture(gl.TEXTURE_2D, null);
		}
	}, {
		key: 'textureWrap',
		value: function textureWrap(_texture, _wrap) {

			var it = this;
			var gl = it.gl;

			gl.bindTexture(gl.TEXTURE_2D, _texture);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, _wrap);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, _wrap);
			gl.bindTexture(gl.TEXTURE_2D, null);
		}
	}, {
		key: 'setTexture',
		value: function setTexture(_texture, _image) {

			var it = this;
			var gl = it.gl;

			gl.bindTexture(gl.TEXTURE_2D, _texture);
			gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, _image);
			gl.bindTexture(gl.TEXTURE_2D, null);
		}
	}, {
		key: 'setTextureFromArray',
		value: function setTextureFromArray(_texture, _width, _height, _array) {

			var it = this;
			var gl = it.gl;

			gl.bindTexture(gl.TEXTURE_2D, _texture);
			gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, _width, _height, 0, gl.RGBA, gl.UNSIGNED_BYTE, new Uint8Array(_array));
			gl.bindTexture(gl.TEXTURE_2D, null);
		}
	}, {
		key: 'setTextureFromFloatArray',
		value: function setTextureFromFloatArray(_texture, _width, _height, _array) {

			var it = this;
			var gl = it.gl;

			gl.bindTexture(gl.TEXTURE_2D, _texture);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
			gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, _width, _height, 0, gl.RGBA, gl.FLOAT, new Float32Array(_array));
			gl.bindTexture(gl.TEXTURE_2D, null);
		}
	}, {
		key: 'copyTexture',
		value: function copyTexture(_texture, _width, _height) {

			var it = this;
			var gl = it.gl;

			gl.bindTexture(gl.TEXTURE_2D, _texture);
			gl.copyTexImage2D(gl.TEXTURE_2D, 0, gl.RGBA, 0, 0, _width, _height, 0);
			gl.bindTexture(gl.TEXTURE_2D, null);
		}
	}, {
		key: 'createCubemap',
		value: function createCubemap(_arrayOfImage) {

			var it = this;
			var gl = it.gl;

			// order : X+, X-, Y+, Y-, Z+, Z-
			var texture = gl.createTexture();

			gl.bindTexture(gl.TEXTURE_CUBE_MAP, texture);
			for (var i = 0; i < 6; i++) {
				gl.texImage2D(gl.TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, _arrayOfImage[i]);
			}
			gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
			gl.texParameteri(gl.TEXTURE_CUBE_MAP, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
			gl.texParameteri(gl.TEXTURE_CUBE_MAP, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
			gl.texParameteri(gl.TEXTURE_CUBE_MAP, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
			gl.texParameteri(gl.TEXTURE_CUBE_MAP, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
			gl.bindTexture(gl.TEXTURE_CUBE_MAP, null);

			return texture;
		}
	}, {
		key: 'createFramebuffer',
		value: function createFramebuffer(_width, _height) {

			var it = this;
			var gl = it.gl;

			var framebuffer = gl.createFramebuffer();
			gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);

			framebuffer.depth = gl.createRenderbuffer();
			gl.bindRenderbuffer(gl.RENDERBUFFER, framebuffer.depth);
			gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, _width, _height);
			gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.RENDERBUFFER, framebuffer.depth);

			framebuffer.texture = it.createTexture();
			gl.bindTexture(gl.TEXTURE_2D, framebuffer.texture);
			gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, _width, _height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
			gl.bindTexture(gl.TEXTURE_2D, null);

			gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, framebuffer.texture, 0);
			gl.bindFramebuffer(gl.FRAMEBUFFER, null);

			return framebuffer;
		}
	}, {
		key: 'resizeFramebuffer',
		value: function resizeFramebuffer(_framebuffer, _width, _height) {

			var it = this;
			var gl = it.gl;

			gl.bindFramebuffer(gl.FRAMEBUFFER, _framebuffer);
			gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, _width, _height);
			gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, _width, _height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
			gl.bindFramebuffer(gl.FRAMEBUFFER, null);
		}
	}, {
		key: 'createFloatFramebuffer',
		value: function createFloatFramebuffer(_width, _height) {

			var it = this;
			var gl = it.gl;

			var framebuffer = gl.createFramebuffer();
			gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);

			framebuffer.depth = gl.createRenderbuffer();
			gl.bindRenderbuffer(gl.RENDERBUFFER, framebuffer.depth);
			gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, _width, _height);
			gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.RENDERBUFFER, framebuffer.depth);

			framebuffer.texture = it.createTexture();
			gl.bindTexture(gl.TEXTURE_2D, framebuffer.texture);
			gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, _width, _height, 0, gl.RGBA, gl.FLOAT, null);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
			gl.bindTexture(gl.TEXTURE_2D, null);

			gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, framebuffer.texture, 0);
			gl.bindFramebuffer(gl.FRAMEBUFFER, null);

			return framebuffer;
		}
	}, {
		key: 'resizeFloatFramebuffer',
		value: function resizeFloatFramebuffer(_framebuffer, _width, _height) {

			var it = this;
			var gl = it.gl;

			gl.bindFramebuffer(gl.FRAMEBUFFER, _framebuffer);
			gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, _width, _height);
			gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, _width, _height, 0, gl.RGBA, gl.FLOAT, null);
			gl.bindFramebuffer(gl.FRAMEBUFFER, null);
		}
	}, {
		key: 'clear',
		value: function clear(_r, _g, _b, _a, _d) {

			var it = this;
			var gl = it.gl;

			var r = _r || 0.0;
			var g = _g || 0.0;
			var b = _b || 0.0;
			var a = typeof _a === 'number' ? _a : 1.0;
			var d = typeof _d === 'number' ? _d : 1.0;

			gl.clearColor(r, g, b, a);
			gl.clearDepth(d);
			gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
		}
	}]);

	return GLCat;
}();

exports.default = GLCat;

},{}],"/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160908/src/script/main.js":[function(require,module,exports){
'use strict';

var _xorshift = require('./xorshift');

var _xorshift2 = _interopRequireDefault(_xorshift);

var _glcat = require('./glcat');

var _glcat2 = _interopRequireDefault(_glcat);

var _step = require('./step');

var _step2 = _interopRequireDefault(_step);

var _tweak = require('./tweak');

var _tweak2 = _interopRequireDefault(_tweak);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

(0, _xorshift2.default)(187);




// ---

var clamp = function clamp(_value, _min, _max) {
  return Math.min(Math.max(_value, _min), _max);
};
var saturate = function saturate(_value) {
  return clamp(_value, 0.0, 1.0);
};

// ---

var width = canvas.width = 300;
var height = canvas.height = 300;

var gl = canvas.getContext('webgl');
var glCat = new _glcat2.default(gl);

// ---

var tweak = new _tweak2.default(divTweak);

// ---

var frame = 0;
var frames = 120;
var iSample = 0;
var nSample = 1;
var time = 0.0;
var deltaTime = 1.0 / 60.0;

var timeUpdate = function timeUpdate() {
  var timePrev = time;
  time = (frame + iSample / nSample) / frames;
  time = time % 1.0;
};

// ---

var vboQuad = glCat.createVertexbuffer([-1, -1, 1, -1, -1, 1, 1, 1]);

// ---

var vertQuad = "#define GLSLIFY 1\nattribute vec2 p;\n\nvoid main() {\n  gl_Position = vec4( p, 0.0, 1.0 );\n}\n";

var programReturn = glCat.createProgram(vertQuad, "precision highp float;\n#define GLSLIFY 1\n\nuniform vec2 resolution;\nuniform sampler2D texture;\n\nvoid main() {\n  vec2 uv = gl_FragCoord.xy / resolution;\n  gl_FragColor = texture2D( texture, uv );\n}\n");

var programRaymarch = glCat.createProgram(vertQuad, "#define MARCH_ITER 70\n#define RAYAMP_MIN 0.0\n#define INIT_LEN 0.01\n#define NSAMPLE 2\n#define NREF 10\n#define SKY_COLOR vec3( 0.0 )\n\n// ---\n\n#define PI 3.14159265\n#define V vec2(0.,1.)\n#define saturate(i) clamp(i,0.,1.)\n#define lofi(i,m) (floor((i)/(m))*(m))\n\n// ---\n\nprecision highp float;\n#define GLSLIFY 1\n\nuniform float time;\nuniform vec2 resolution;\n\nuniform sampler2D textureRandom;\nuniform sampler2D textureRandomStatic;\n\n// ---\n\nfloat phase;\nvec3 color;\nvec3 amp;\n\n// ---\n\nvec4 seed;\nfloat random() { // weird prng\n  const vec4 q = vec4(   1225.0,    1585.0,    2457.0,    2098.0);\n  const vec4 r = vec4(   1112.0,     367.0,      92.0,     265.0);\n  const vec4 a = vec4(   3423.0,    2646.0,    1707.0,    1999.0);\n  const vec4 m = vec4(4194287.0, 4194277.0, 4194191.0, 4194167.0);\n\n  vec4 beta = floor(seed / q);\n  vec4 p = a * (seed - beta * q) - beta * r;\n  beta = (sign(-p) + vec4(1.0)) * vec4(0.5) * m;\n  seed = (p + beta);\n\n  return fract(dot(seed / m, vec4(1.0, -1.0, 1.0, -1.0)));\n}\n\nvec4 random4() {\n  return vec4(\n    random(),\n    random(),\n    random(),\n    random()\n  );\n}\n\n// ---\n\nmat2 rotate2D( float _t ) {\n  return mat2( cos( _t ), sin( _t ), -sin( _t ), cos( _t ) );\n}\n\nvec3 rotateEuler( vec3 _p, vec3 _r ) {\n  vec3 p = _p;\n  p.yz = rotate2D( _r.x ) * p.yz;\n  p.zx = rotate2D( _r.y ) * p.zx;\n  p.xy = rotate2D( _r.z ) * p.xy;\n  return p;\n}\n\n// ---\n\nstruct Camera {\n  vec3 pos;\n  vec3 dir;\n  vec3 sid;\n  vec3 top;\n};\n\nstruct Ray {\n  vec3 dir;\n  vec3 ori;\n  bool inside;\n};\n\nstruct Material {\n  vec3 color;\n\n  vec3 emissive;\n  vec3 edgeEmissive;\n\n  float reflective;\n  float reflectiveRoughness;\n  float refractive;\n  float refractiveIndex;\n\n  float transparency;\n};\n\nstruct Map {\n  float dist;\n  Material material;\n};\n\nstruct March {\n  Ray ray;\n  Map map;\n  float len;\n  vec3 pos;\n  vec3 normal;\n};\n\n// ---\n\nCamera camInit( in vec3 _pos, in vec3 _tar ) {\n  Camera cam;\n  cam.pos = _pos;\n  cam.dir = normalize( _tar - _pos );\n  cam.sid = normalize( cross( cam.dir, V.xyx ) );\n  cam.top = normalize( cross( cam.sid, cam.dir ) );\n\n  return cam;\n}\n\nMap distFunc( in vec3 _p );\nRay rayInit( in vec3 _ori, in vec3 _dir ) {\n  Ray ray;\n  ray.dir = _dir;\n  ray.ori = _ori;\n  ray.inside = distFunc( ray.ori ).dist < 0.0;\n  return ray;\n}\n\nRay rayFromCam( in vec2 _p, in Camera _cam ) {\n  vec3 dir = normalize( _p.x * _cam.sid + _p.y * _cam.top + _cam.dir * 3.2 );\n  return rayInit( _cam.pos, dir );\n}\n\nMaterial mtlInit( in vec3 _col ) {\n  Material material;\n  material.color = _col;\n\n  material.emissive = V.xxx;\n  material.edgeEmissive = V.xxx;\n\n  material.reflective = 0.0;\n  material.reflectiveRoughness = 0.0;\n  material.refractive = 0.0;\n  material.refractiveIndex = 1.0;\n\n  material.transparency = 0.0;\n\n  return material;\n}\n\nMap mapInit( in float _dist ) {\n  Map map;\n  map.dist = _dist;\n  map.material = mtlInit( V.xxx );\n  return map;\n}\n\nMarch marchInit( in Ray _ray ) {\n  March march;\n  march.ray = _ray;\n  march.len = INIT_LEN;\n  march.pos = _ray.ori + _ray.dir * march.len;\n  return march;\n}\n\n// ---\n\nfloat sphere( in vec3 _p, in float _r ) {\n  return length( _p ) - _r;\n}\n\nfloat box( in vec3 _pos, in vec3 _size ) {\n  vec3 d = abs( _pos ) - _size;\n  return min( max( d.x, max( d.y, d.z ) ), 0.0 ) + length( max( d, 0.0 ) );\n}\n\nbool cornellBox( inout Map map, in vec3 _pos, in float _size ) {\n  vec3 p = _pos / _size;\n  bool ret = false;\n\n  { // box\n    vec3 p2 = p;\n    float dist = box( p2, vec3( 1.05 ) );\n    dist = max( dist, -box( p2 - vec3( 0.0, 0.0, 0.05 ), vec3( 1.0, 1.0, 1.05 ) ) );\n    dist = max( dist, -box( p2 - vec3( 0.0, 1.0, 0.0 ), vec3( 0.2 * 1.05 ) ) );\n\n    {\n      vec3 p3 = p2 - vec3( 0.3, -0.4, -0.5 );\n      p3.zx = rotate2D( 0.4 ) * p3.zx;\n      dist = min( dist, box( p3, vec3( 0.3, 0.6, 0.3 ) ) );\n    }\n\n    if ( dist * _size < map.dist ) {\n      map = mapInit( dist * _size );\n      map.material = mtlInit(\n        p.x < -0.99 ? vec3( 0.9, 0.1, 0.1 )\n        : 0.99 < p.x ? vec3( 0.0, 0.9, 0.1 )\n        : vec3( 0.9 )\n      );\n\n      ret = true;\n    }\n  }\n\n  { // sph\n    vec3 p2 = p - vec3( -0.4, -0.6, -0.1 );\n    float dist = sphere( p2, 0.4 );\n\n    if ( dist * _size < map.dist ) {\n      map = mapInit( dist * _size );\n      map.material = mtlInit( vec3( 1.0 ) );\n      map.material.reflective = 1.0;\n    }\n  }\n\n  { // sph\n    vec3 p2 = p - vec3( 0.4, -0.7, 0.4 );\n    float dist = sphere( p2, 0.3 );\n\n    if ( dist * _size < map.dist ) {\n      map = mapInit( dist * _size );\n      map.material = mtlInit( vec3( 1.0 ) );\n      map.material.refractive = 1.0;\n      map.material.refractiveIndex = 1.5;\n    }\n  }\n\n  return ret;\n}\n\nMap distFunc( in vec3 _p, in float _time ) {\n  Map map = mapInit( 1E9 );\n  float x5 = 1.0 + 4.0 * phase;\n\n  cornellBox( map, _p, 1.0 * x5 );\n\n  {\n    vec3 p = _p;\n    p -= vec3( 0.0, 1.2, 0.0 ) * x5;\n    p.yz = rotate2D( -PI / 2.0 ) * p.yz;\n    bool result = cornellBox( map, p, 0.2 * x5 );\n    if ( result ) {\n      map.material.emissive = vec3( 65.0, 60.0, 55.0 ) * exp( -phase * 5.0 ) * ( 1.0 - phase );\n    }\n\n    {\n      vec3 p2 = p;\n      p2 -= vec3( 0.0, 1.2, 0.0 ) / 5.0 * x5;\n      p2.yz = rotate2D( -PI / 2.0 ) * p2.yz;\n      bool result = cornellBox( map, p2, 0.2 / 5.0 * x5 );\n      if ( result ) {\n        map.material.emissive = vec3( 65.0, 60.0, 55.0 );\n      }\n    }\n  }\n\n  return map;\n}\n\nMap distFunc( in vec3 _p ) {\n  return distFunc( _p, time );\n}\n\nvec3 normalFunc( in vec3 _p, in float _d ) {\n  vec2 d = V * _d;\n  return normalize( vec3(\n    distFunc( _p + d.yxx ).dist - distFunc( _p - d.yxx ).dist,\n    distFunc( _p + d.xyx ).dist - distFunc( _p - d.xyx ).dist,\n    distFunc( _p + d.xxy ).dist - distFunc( _p - d.xxy ).dist\n  ) );\n}\n\n// ---\n\nMarch march( in Ray _ray ) {\n  Ray ray = _ray;\n  March march = marchInit( ray );\n\n  for ( int iMarch = 0; iMarch < MARCH_ITER; iMarch ++ ) {\n    Map map = distFunc( march.pos );\n    map.dist *= ( ray.inside ? -1.0 : 1.0 ) * 0.8;\n\n    march.map = map;\n    march.len += map.dist;\n    march.pos = ray.ori + ray.dir * march.len;\n\n    if ( 1E3 < march.len || abs( map.dist ) < INIT_LEN * 0.01 ) { break; }\n  }\n\n  march.normal = normalFunc( march.pos, 1E-4 );\n\n  return march;\n}\n\n// ---\n\nvec3 randomSphere() {\n  vec3 dir = V.xxx;\n  for ( int i = 0; i < 9; i ++ ) {\n    dir = random4().xyz * 2.0 - 1.0;\n    if ( length( dir ) < 1.0 ) { break; }\n  }\n  dir = normalize( dir );\n  return dir;\n}\n\nvec3 randomHemisphere( in vec3 _normal ) {\n  vec3 dir = randomSphere();\n  if ( dot( dir, _normal ) < 0.0 ) { dir = -dir; }\n  return dir;\n}\n\nRay shade( in March _march ) {\n  March march = _march;\n\n  if ( abs( march.map.dist ) < 1E-2 ) {\n    bool inside = march.ray.inside;\n    vec3 normal = march.normal;\n    float edge = length( saturate( ( normalFunc( march.pos, 4E-4 ) - normal ) * 4.0 ) );\n\n    normal = inside ? -normal : normal;\n    Material material = march.map.material;\n\n    vec3 dir = V.xxx;\n    float dice = random4().x;\n\n    // color += amp * max( 0.0, dot( normal, -march.ray.dir ) ) * march.map.material.emissive;\n    color += amp * march.map.material.emissive;\n    color += amp * edge * march.map.material.edgeEmissive;\n\n    amp *= mix( march.map.material.color, V.yyy, march.map.material.transparency );\n    if ( dice < material.reflective ) { // reflect\n      vec3 ref = normalize( reflect(\n        march.ray.dir,\n        normal\n      ) );\n      vec3 dif = randomHemisphere( normal );\n      dir = normalize( mix(\n        ref,\n        dif,\n        material.reflectiveRoughness\n      ) );\n      amp *= max( dot( dir, dif ), 0.0 );\n\n    } else if ( dice < material.reflective + material.refractive ) { // refract\n      vec3 inc = normalize( march.ray.dir );\n      bool toAir = ( 0.0 < dot( normal, inc ) );\n      float eta = 1.0 / march.map.material.refractiveIndex;\n      eta = inside ? 1.0 / eta : eta;\n\n      dir = refract(\n        inc,\n        toAir ? -normal : normal,\n        toAir ? 1.0 / eta : eta\n      );\n      dir = ( dir == V.xxx )\n      ? ( normalize( reflect(\n        march.ray.dir,\n        normal\n      ) ) )\n      : normalize( dir );\n      inside = !inside;\n\n    } else { // diffuse\n      dir = randomHemisphere( normal );\n      amp *= mix( max( dot( dir, normal ), 0.0 ), 1.0, march.map.material.transparency );\n    }\n\n    Ray ray = rayInit( march.pos, dir );\n    ray.inside = inside;\n    return ray;\n  } else {\n    color += amp * SKY_COLOR;\n    amp *= 0.0;\n\n    return rayInit( V.xxx, V.xxx );\n  }\n}\n\n// ---\n\nvoid main() {\n  seed = texture2D( textureRandom, gl_FragCoord.xy / resolution );\n\n  phase = ( 1.0 - cos( time * PI ) ) * 0.5;\n  vec3 sum = V.xxx;\n\n  for ( int iSample = 0; iSample < NSAMPLE; iSample ++ ) {\n    Camera cam = camInit(\n      vec3( 0.0, 2.0 * phase, 4.0 * ( 1.0 - phase ) ),\n      vec3( 0.0, 4.0 * phase, 0.0 )\n    );\n    cam.pos += ( random4().y - 0.5 ) * 0.01 * cam.sid;\n    cam.pos += ( random4().y - 0.5 ) * 0.01 * cam.top;\n\n    vec2 pix = gl_FragCoord.xy + random4().xy - 0.5;\n    vec2 p = ( pix * 2.0 - resolution ) / resolution.x;\n    Ray ray = rayFromCam( p, cam );\n\n    color = V.xxx;\n    amp = V.yyy;\n\n    for ( int iRef = 0; iRef < NREF; iRef ++ ) {\n      ray = shade( march( ray ) );\n\n      if ( length( amp ) < RAYAMP_MIN ) { break; }\n    }\n\n    sum += color / float( NSAMPLE );\n  }\n\n  gl_FragColor = vec4( sum, 1.0 );\n}\n");

var programSum = glCat.createProgram(vertQuad, "precision highp float;\n#define GLSLIFY 1\n\nuniform bool init;\nuniform float add;\nuniform vec2 resolution;\nuniform sampler2D textureAdd;\nuniform sampler2D textureBase;\n\nvoid main() {\n  vec2 uv = gl_FragCoord.xy / resolution;\n  vec3 ret = texture2D( textureAdd, uv ).xyz * add;\n  if ( !init ) {\n    ret += texture2D( textureBase, uv ).xyz;\n  }\n  gl_FragColor = vec4( ret, 1.0 );\n}\n");

var programGamma = glCat.createProgram(vertQuad, "precision highp float;\n#define GLSLIFY 1\n\nuniform vec2 resolution;\nuniform sampler2D texture;\n\nvoid main() {\n  vec2 uv = gl_FragCoord.xy / resolution;\n  gl_FragColor = vec4(\n    pow( texture2D( texture, uv ).xyz, vec3( 1.0 / 1.0 ) ),\n    1.0\n  );\n}\n");

// ---

var framebufferReturn = glCat.createFloatFramebuffer(width, height);
var framebufferRaymarch = glCat.createFloatFramebuffer(width, height);
var framebufferSum = glCat.createFloatFramebuffer(width, height);

// ---

var textureRandomSize = 256;

var textureRandomUpdate = function textureRandomUpdate(_tex) {
  glCat.setTextureFromArray(_tex, textureRandomSize, textureRandomSize, function () {
    var len = textureRandomSize * textureRandomSize * 4;
    var ret = new Uint8Array(len);
    for (var i = 0; i < len; i++) {
      ret[i] = Math.floor((0, _xorshift2.default)() * 256.0);
    }
    return ret;
  }());
};

var textureRandom = glCat.createTexture();
glCat.textureWrap(textureRandom, gl.REPEAT);

var textureRandomStatic = glCat.createTexture();
glCat.textureWrap(textureRandomStatic, gl.REPEAT);
textureRandomUpdate(textureRandomStatic);

// ---

var renderA = document.createElement('a');

var saveFrame = function saveFrame() {
  renderA.href = canvas.toDataURL();
  renderA.download = ('0000' + frame).slice(-5) + '.png';
  renderA.click();
};

// ---

var render = function render() {
  gl.viewport(0, 0, width, height);
  glCat.useProgram(programRaymarch);
  gl.bindFramebuffer(gl.FRAMEBUFFER, framebufferRaymarch);
  glCat.clear();

  glCat.attribute('p', vboQuad, 2);

  glCat.uniform1f('time', time);
  glCat.uniform2fv('resolution', [width, height]);

  glCat.uniformTexture('textureRandom', textureRandom, 0);
  glCat.uniformTexture('textureRandomStatic', textureRandomStatic, 1);

  gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);

  // ---

  gl.viewport(0, 0, width, height);
  glCat.useProgram(programReturn);
  gl.bindFramebuffer(gl.FRAMEBUFFER, framebufferReturn);
  glCat.clear();

  glCat.attribute('p', vboQuad, 2);
  glCat.uniform2fv('resolution', [width, height]);
  glCat.uniformTexture('texture', framebufferSum.texture, 0);

  gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);

  // ---

  gl.viewport(0, 0, width, height);
  glCat.useProgram(programSum);
  gl.bindFramebuffer(gl.FRAMEBUFFER, framebufferSum);
  glCat.clear();

  glCat.attribute('p', vboQuad, 2);
  glCat.uniform1f('add', 1.0 / nSample);
  glCat.uniform1i('init', iSample === 0);
  glCat.uniform2fv('resolution', [width, height]);
  glCat.uniformTexture('textureBase', framebufferReturn.texture, 0);
  glCat.uniformTexture('textureAdd', framebufferRaymarch.texture, 1);

  gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
};

// ---

var preview = function preview() {
  gl.viewport(0, 0, width, height);
  glCat.useProgram(programReturn);
  gl.bindFramebuffer(gl.FRAMEBUFFER, null);
  glCat.clear();

  glCat.attribute('p', vboQuad, 2);
  glCat.uniform2fv('resolution', [width, height]);
  glCat.uniformTexture('texture', framebufferSum.texture, 0);

  gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
};

// ---

var post = function post() {
  gl.viewport(0, 0, width, height);
  glCat.useProgram(programGamma);
  gl.bindFramebuffer(gl.FRAMEBUFFER, null);
  glCat.clear();

  glCat.attribute('p', vboQuad, 2);
  glCat.uniform2fv('resolution', [width, height]);
  glCat.uniformTexture('texture', framebufferSum.texture, 0);

  gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
};

// ---

var update = function update() {
  if (!checkboxPlay.checked) {
    setTimeout(update, 10);
    return;
  }

  timeUpdate();
  textureRandomUpdate(textureRandom);

  render();

  iSample++;
  if (iSample === nSample) {
    iSample = 0;
    console.log(frame);

    post();

    if (checkboxSave.checked) {
      saveFrame();
    }
    nSample = Math.floor(tweak.range('nSample', { value: 600.0, min: 1.0, max: 1000.0, step: 1.0 }));
    frame++;
  } else {
    preview();
  }

  requestAnimationFrame(update);
};

// ---

(0, _step2.default)({
  0: function _(done) {
    update();
  }
});

window.addEventListener('keydown', function (_e) {
  if (_e.which === 27) {
    checkboxPlay.checked = false;
  }
});

},{"./glcat":"/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160908/src/script/glcat.js","./step":"/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160908/src/script/step.js","./tweak":"/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160908/src/script/tweak.js","./xorshift":"/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160908/src/script/xorshift.js"}],"/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160908/src/script/step.js":[function(require,module,exports){
'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
var step = function step(_obj) {
  var obj = _obj;
  var count = -1;

  var func = function func() {
    count++;
    if (typeof obj[count] === 'function') {
      obj[count](func);
    }
  };
  func();
};

exports.default = step;

},{}],"/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160908/src/script/tweak.js":[function(require,module,exports){
'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var Tweak = function () {
  function Tweak(_el) {
    _classCallCheck(this, Tweak);

    var it = this;

    it.parent = _el;
    it.values = {};
    it.elements = {};
  }

  _createClass(Tweak, [{
    key: 'button',
    value: function button(_name, _props) {
      var it = this;

      var props = _props || {};

      if (typeof it.values[_name] === 'undefined') {
        var div = document.createElement('div');
        it.parent.appendChild(div);

        var input = document.createElement('input');
        div.appendChild(input);
        input.type = 'button';
        input.value = _name;

        input.addEventListener('click', function () {
          it.values[_name] = true;
        });

        it.elements[_name] = {
          div: div,
          input: input
        };
      }

      var tempvalue = it.values[_name];
      it.values[_name] = false;
      if (typeof props.set === 'boolean') {
        it.values[_name] = props.set;
      }

      return tempvalue;
    }
  }, {
    key: 'checkbox',
    value: function checkbox(_name, _props) {
      var it = this;

      var props = _props || {};

      var value = void 0;

      if (typeof it.values[_name] === 'undefined') {
        value = props.value || false;

        var div = document.createElement('div');
        it.parent.appendChild(div);

        var name = document.createElement('span');
        div.appendChild(name);
        name.innerText = _name;

        var input = document.createElement('input');
        div.appendChild(input);
        input.type = 'checkbox';
        input.checked = value;

        it.elements[_name] = {
          div: div,
          name: name,
          input: input
        };
      } else {
        value = it.elements[_name].input.checked;
      }

      if (typeof props.set === 'boolean') {
        value = props.set;
      }

      it.elements[_name].input.checked = value;
      it.values[_name] = value;

      return it.values[_name];
    }
  }, {
    key: 'range',
    value: function range(_name, _props) {
      var it = this;

      var props = _props || {};

      var value = void 0;

      if (typeof it.values[_name] === 'undefined') {
        var min = props.min || 0.0;
        var max = props.max || 1.0;
        var step = props.step || 0.001;
        value = props.value || min;

        var div = document.createElement('div');
        it.parent.appendChild(div);

        var name = document.createElement('span');
        div.appendChild(name);
        name.innerText = _name;

        var input = document.createElement('input');
        div.appendChild(input);
        input.type = 'range';
        input.value = value;
        input.min = min;
        input.max = max;
        input.step = step;

        var val = document.createElement('span');
        div.appendChild(val);

        it.elements[_name] = {
          div: div,
          name: name,
          input: input,
          val: val
        };
      } else {
        value = parseFloat(it.elements[_name].input.value);
      }

      if (typeof props.set === 'number') {
        value = props.set;
      }

      it.values[_name] = value;
      it.elements[_name].input.value = value;
      it.elements[_name].val.innerText = value.toFixed(3);

      return it.values[_name];
    }
  }]);

  return Tweak;
}();

exports.default = Tweak;

},{}],"/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160908/src/script/xorshift.js":[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
var seed = void 0;
var xorshift = function xorshift(_seed) {
  seed = _seed || seed || 1;
  seed = seed ^ seed << 13;
  seed = seed ^ seed >>> 17;
  seed = seed ^ seed << 5;
  return seed / Math.pow(2, 32) + 0.5;
};

exports.default = xorshift;

},{}]},{},["/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160908/src/script/main.js"]);
