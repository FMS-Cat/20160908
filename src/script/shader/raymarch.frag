#define MARCH_ITER 70
#define RAYAMP_MIN 0.0
#define INIT_LEN 0.01
#define NSAMPLE 2
#define NREF 10
#define SKY_COLOR vec3( 0.0 )

// ---

#define PI 3.14159265
#define V vec2(0.,1.)
#define saturate(i) clamp(i,0.,1.)
#define lofi(i,m) (floor((i)/(m))*(m))

// ---

precision highp float;

uniform float time;
uniform vec2 resolution;

uniform sampler2D textureRandom;
uniform sampler2D textureRandomStatic;

// ---

float phase;
vec3 color;
vec3 amp;

// ---

vec4 seed;
float random() { // weird prng
  const vec4 q = vec4(   1225.0,    1585.0,    2457.0,    2098.0);
  const vec4 r = vec4(   1112.0,     367.0,      92.0,     265.0);
  const vec4 a = vec4(   3423.0,    2646.0,    1707.0,    1999.0);
  const vec4 m = vec4(4194287.0, 4194277.0, 4194191.0, 4194167.0);

  vec4 beta = floor(seed / q);
  vec4 p = a * (seed - beta * q) - beta * r;
  beta = (sign(-p) + vec4(1.0)) * vec4(0.5) * m;
  seed = (p + beta);

  return fract(dot(seed / m, vec4(1.0, -1.0, 1.0, -1.0)));
}

vec4 random4() {
  return vec4(
    random(),
    random(),
    random(),
    random()
  );
}

// ---

mat2 rotate2D( float _t ) {
  return mat2( cos( _t ), sin( _t ), -sin( _t ), cos( _t ) );
}

vec3 rotateEuler( vec3 _p, vec3 _r ) {
  vec3 p = _p;
  p.yz = rotate2D( _r.x ) * p.yz;
  p.zx = rotate2D( _r.y ) * p.zx;
  p.xy = rotate2D( _r.z ) * p.xy;
  return p;
}

// ---

struct Camera {
  vec3 pos;
  vec3 dir;
  vec3 sid;
  vec3 top;
};

struct Ray {
  vec3 dir;
  vec3 ori;
  bool inside;
};

struct Material {
  vec3 color;

  vec3 emissive;
  vec3 edgeEmissive;

  float reflective;
  float reflectiveRoughness;
  float refractive;
  float refractiveIndex;

  float transparency;
};

struct Map {
  float dist;
  Material material;
};

struct March {
  Ray ray;
  Map map;
  float len;
  vec3 pos;
  vec3 normal;
};

// ---

Camera camInit( in vec3 _pos, in vec3 _tar ) {
  Camera cam;
  cam.pos = _pos;
  cam.dir = normalize( _tar - _pos );
  cam.sid = normalize( cross( cam.dir, V.xyx ) );
  cam.top = normalize( cross( cam.sid, cam.dir ) );

  return cam;
}

Map distFunc( in vec3 _p );
Ray rayInit( in vec3 _ori, in vec3 _dir ) {
  Ray ray;
  ray.dir = _dir;
  ray.ori = _ori;
  ray.inside = distFunc( ray.ori ).dist < 0.0;
  return ray;
}

Ray rayFromCam( in vec2 _p, in Camera _cam ) {
  vec3 dir = normalize( _p.x * _cam.sid + _p.y * _cam.top + _cam.dir * 3.2 );
  return rayInit( _cam.pos, dir );
}

Material mtlInit( in vec3 _col ) {
  Material material;
  material.color = _col;

  material.emissive = V.xxx;
  material.edgeEmissive = V.xxx;

  material.reflective = 0.0;
  material.reflectiveRoughness = 0.0;
  material.refractive = 0.0;
  material.refractiveIndex = 1.0;

  material.transparency = 0.0;

  return material;
}

Map mapInit( in float _dist ) {
  Map map;
  map.dist = _dist;
  map.material = mtlInit( V.xxx );
  return map;
}

March marchInit( in Ray _ray ) {
  March march;
  march.ray = _ray;
  march.len = INIT_LEN;
  march.pos = _ray.ori + _ray.dir * march.len;
  return march;
}

// ---

float sphere( in vec3 _p, in float _r ) {
  return length( _p ) - _r;
}

float box( in vec3 _pos, in vec3 _size ) {
  vec3 d = abs( _pos ) - _size;
  return min( max( d.x, max( d.y, d.z ) ), 0.0 ) + length( max( d, 0.0 ) );
}

bool cornellBox( inout Map map, in vec3 _pos, in float _size ) {
  vec3 p = _pos / _size;
  bool ret = false;

  { // box
    vec3 p2 = p;
    float dist = box( p2, vec3( 1.05 ) );
    dist = max( dist, -box( p2 - vec3( 0.0, 0.0, 0.05 ), vec3( 1.0, 1.0, 1.05 ) ) );
    dist = max( dist, -box( p2 - vec3( 0.0, 1.0, 0.0 ), vec3( 0.2 * 1.05 ) ) );

    {
      vec3 p3 = p2 - vec3( 0.3, -0.4, -0.5 );
      p3.zx = rotate2D( 0.4 ) * p3.zx;
      dist = min( dist, box( p3, vec3( 0.3, 0.6, 0.3 ) ) );
    }

    if ( dist * _size < map.dist ) {
      map = mapInit( dist * _size );
      map.material = mtlInit(
        p.x < -0.99 ? vec3( 0.9, 0.1, 0.1 )
        : 0.99 < p.x ? vec3( 0.0, 0.9, 0.1 )
        : vec3( 0.9 )
      );

      ret = true;
    }
  }

  { // sph
    vec3 p2 = p - vec3( -0.4, -0.6, -0.1 );
    float dist = sphere( p2, 0.4 );

    if ( dist * _size < map.dist ) {
      map = mapInit( dist * _size );
      map.material = mtlInit( vec3( 1.0 ) );
      map.material.reflective = 1.0;
    }
  }

  { // sph
    vec3 p2 = p - vec3( 0.4, -0.7, 0.4 );
    float dist = sphere( p2, 0.3 );

    if ( dist * _size < map.dist ) {
      map = mapInit( dist * _size );
      map.material = mtlInit( vec3( 1.0 ) );
      map.material.refractive = 1.0;
      map.material.refractiveIndex = 1.5;
    }
  }

  return ret;
}

Map distFunc( in vec3 _p, in float _time ) {
  Map map = mapInit( 1E9 );
  float x5 = 1.0 + 4.0 * phase;

  cornellBox( map, _p, 1.0 * x5 );

  {
    vec3 p = _p;
    p -= vec3( 0.0, 1.2, 0.0 ) * x5;
    p.yz = rotate2D( -PI / 2.0 ) * p.yz;
    bool result = cornellBox( map, p, 0.2 * x5 );
    if ( result ) {
      map.material.emissive = vec3( 65.0, 60.0, 55.0 ) * exp( -phase * 5.0 ) * ( 1.0 - phase );
    }

    {
      vec3 p2 = p;
      p2 -= vec3( 0.0, 1.2, 0.0 ) / 5.0 * x5;
      p2.yz = rotate2D( -PI / 2.0 ) * p2.yz;
      bool result = cornellBox( map, p2, 0.2 / 5.0 * x5 );
      if ( result ) {
        map.material.emissive = vec3( 65.0, 60.0, 55.0 );
      }
    }
  }

  return map;
}

Map distFunc( in vec3 _p ) {
  return distFunc( _p, time );
}

vec3 normalFunc( in vec3 _p, in float _d ) {
  vec2 d = V * _d;
  return normalize( vec3(
    distFunc( _p + d.yxx ).dist - distFunc( _p - d.yxx ).dist,
    distFunc( _p + d.xyx ).dist - distFunc( _p - d.xyx ).dist,
    distFunc( _p + d.xxy ).dist - distFunc( _p - d.xxy ).dist
  ) );
}

// ---

March march( in Ray _ray ) {
  Ray ray = _ray;
  March march = marchInit( ray );

  for ( int iMarch = 0; iMarch < MARCH_ITER; iMarch ++ ) {
    Map map = distFunc( march.pos );
    map.dist *= ( ray.inside ? -1.0 : 1.0 ) * 0.8;

    march.map = map;
    march.len += map.dist;
    march.pos = ray.ori + ray.dir * march.len;

    if ( 1E3 < march.len || abs( map.dist ) < INIT_LEN * 0.01 ) { break; }
  }

  march.normal = normalFunc( march.pos, 1E-4 );

  return march;
}

// ---

vec3 randomSphere() {
  vec3 dir = V.xxx;
  for ( int i = 0; i < 9; i ++ ) {
    dir = random4().xyz * 2.0 - 1.0;
    if ( length( dir ) < 1.0 ) { break; }
  }
  dir = normalize( dir );
  return dir;
}

vec3 randomHemisphere( in vec3 _normal ) {
  vec3 dir = randomSphere();
  if ( dot( dir, _normal ) < 0.0 ) { dir = -dir; }
  return dir;
}

Ray shade( in March _march ) {
  March march = _march;

  if ( abs( march.map.dist ) < 1E-2 ) {
    bool inside = march.ray.inside;
    vec3 normal = march.normal;
    float edge = length( saturate( ( normalFunc( march.pos, 4E-4 ) - normal ) * 4.0 ) );

    normal = inside ? -normal : normal;
    Material material = march.map.material;

    vec3 dir = V.xxx;
    float dice = random4().x;

    // color += amp * max( 0.0, dot( normal, -march.ray.dir ) ) * march.map.material.emissive;
    color += amp * march.map.material.emissive;
    color += amp * edge * march.map.material.edgeEmissive;

    amp *= mix( march.map.material.color, V.yyy, march.map.material.transparency );
    if ( dice < material.reflective ) { // reflect
      vec3 ref = normalize( reflect(
        march.ray.dir,
        normal
      ) );
      vec3 dif = randomHemisphere( normal );
      dir = normalize( mix(
        ref,
        dif,
        material.reflectiveRoughness
      ) );
      amp *= max( dot( dir, dif ), 0.0 );

    } else if ( dice < material.reflective + material.refractive ) { // refract
      vec3 inc = normalize( march.ray.dir );
      bool toAir = ( 0.0 < dot( normal, inc ) );
      float eta = 1.0 / march.map.material.refractiveIndex;
      eta = inside ? 1.0 / eta : eta;

      dir = refract(
        inc,
        toAir ? -normal : normal,
        toAir ? 1.0 / eta : eta
      );
      dir = ( dir == V.xxx )
      ? ( normalize( reflect(
        march.ray.dir,
        normal
      ) ) )
      : normalize( dir );
      inside = !inside;

    } else { // diffuse
      dir = randomHemisphere( normal );
      amp *= mix( max( dot( dir, normal ), 0.0 ), 1.0, march.map.material.transparency );
    }

    Ray ray = rayInit( march.pos, dir );
    ray.inside = inside;
    return ray;
  } else {
    color += amp * SKY_COLOR;
    amp *= 0.0;

    return rayInit( V.xxx, V.xxx );
  }
}

// ---

void main() {
  seed = texture2D( textureRandom, gl_FragCoord.xy / resolution );

  phase = ( 1.0 - cos( time * PI ) ) * 0.5;
  vec3 sum = V.xxx;

  for ( int iSample = 0; iSample < NSAMPLE; iSample ++ ) {
    Camera cam = camInit(
      vec3( 0.0, 2.0 * phase, 4.0 * ( 1.0 - phase ) ),
      vec3( 0.0, 4.0 * phase, 0.0 )
    );
    cam.pos += ( random4().y - 0.5 ) * 0.01 * cam.sid;
    cam.pos += ( random4().y - 0.5 ) * 0.01 * cam.top;

    vec2 pix = gl_FragCoord.xy + random4().xy - 0.5;
    vec2 p = ( pix * 2.0 - resolution ) / resolution.x;
    Ray ray = rayFromCam( p, cam );

    color = V.xxx;
    amp = V.yyy;

    for ( int iRef = 0; iRef < NREF; iRef ++ ) {
      ray = shade( march( ray ) );

      if ( length( amp ) < RAYAMP_MIN ) { break; }
    }

    sum += color / float( NSAMPLE );
  }

  gl_FragColor = vec4( sum, 1.0 );
}
