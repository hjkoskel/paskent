#version 330
uniform vec2 resolution; // Width & height of the shader
uniform float u_time; // Time elapsed
uniform float phi;
uniform float theta;
uniform vec3 player;

uniform sampler2D texbrick;
uniform sampler2D texbrick2;
uniform sampler2D texwater;

// Constants
#define PI 3.1415925359
#define TWO_PI 6.2831852
#define MAX_STEPS 200 // Mar Raymarching steps
#define MAX_DIST 100. // Max Raymarching distance
#define SURF_DIST .01 // Surface Distance


vec4 minsdf( vec4 a, vec4 b ){
    return (a.x<b.x) ? a : b;
}

float sdSphere(vec3 p,float s){
  return length(p)-s;
}

float sdTorus( vec3 p, vec2 t ){
  vec2 q = vec2(length(p.xz)-t.x,p.y);
  return length(q)-t.y;
}

float smin(float a, float b) {
 float k = 0.2;
 float h = clamp(0.5 + 0.5 * (b - a) / k, 0,1);
 return mix(b, a, h) - k * h * (1 - h);
}

vec4 sminsdf(vec4 a, vec4 b,float k) {
 //float k = 0.2;
 float h = clamp(0.5 + 0.5 * (b.x - a.x) / k, 0,1);

 float d=mix(b.x, a.x, h) - k * h * (1 - h);
 return (a.x<b.x) ? vec4(d,a.gba) : vec4(d,b.gba);
}

float sdBox(vec3 p,vec3 b){
  vec3 q = abs(p) - b;
  return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
}

vec3 rotateVector(vec3 v, vec4 q){
	return v + 2.0 * cross(q.xyz, cross(q.xyz, v) + q.w * v);
}


// A simple 2D Perlin noise function
float noise(vec2 p) {
    return fract(sin(dot(p, vec2(12.9898, 78.233))) * 43758.5453);
}

// Smooth interpolation function
float interpolate(float a, float b, float t) {
    return mix(a, b, t * t * (3.0 - 2.0 * t));
}

// 2D Perlin noise function
float perlinNoise(vec2 st) {
    vec2 i = floor(st);
    vec2 f = fract(st);

    // Get the noise values at the four corners of the grid cell
    float a = noise(i);
    float b = noise(i + vec2(1.0, 0.0));
    float c = noise(i + vec2(0.0, 1.0));
    float d = noise(i + vec2(1.0, 1.0));

    // Interpolate between the noise values
    float u = interpolate(a, b, f.x);
    float v = interpolate(c, d, f.x);
    return interpolate(u, v, f.y);
}

// Fractal noise with high-value lumps
float fractalNoise(vec2 uv, int octaves, float persistence) {
    float total = 0.0;
    float frequency = 1.0;
    float amplitude = 1.0;
    float maxValue = 0.0; // Used for normalizing the result to [0, 1]

    for (int i = 0; i < octaves; i++) {
        total += perlinNoise(uv * frequency) * amplitude;

        maxValue += amplitude; // Accumulate the maximum possible value
        frequency *= 2.0;      // Double the frequency for the next octave
        amplitude *= persistence; // Reduce the amplitude for the next octave
    }

    // Normalize the result to [0, 1]
    return total / maxValue;
}


vec4 sdf_sea(vec3 p){
    int octaves = 3; // Number of noise layers
    float persistence = 0.1; // Controls the amplitude decay of each octave
    
    return vec4(p.y-fractalNoise(vec2(p.x,p.z), octaves, persistence)/5,1,p.x,p.z );
    //return vec4(p.y-perlinNoise(vec2(p.x,p.z)),1,p.x,p.z );
   //return vec4(p.y-sin(p.x/2+p.y/10)*cos(p.z) +1 ,1,p.x,p.z);
}

vec4 sdf_land(vec3 p){
    int octaves = 5; // Number of noise layers
    float persistence = 0.2; // Controls the amplitude decay of each octave
    float h1=fractalNoise(vec2(p.x/10,p.z/10),octaves, persistence);
    float h2=fractalNoise(vec2(p.x/60,p.z/60),octaves, persistence);

    float h=h1*h2*20;
    return vec4(p.y-h+0.5,2,h/20,0);
}

vec4 sdf_sphere0(vec3 p){
    vec3 spherePos2=vec3(1,2.3,6);
    float sphereDist2 = length(p-spherePos2) - 1;
    return vec4(sphereDist2, 3, p.x, p.y);
}

float length8( vec2 p ){
	p = p*p; p = p*p; p = p*p;
	return pow( p.x + p.y, 1.0/8.0 );
}


float length2( vec2 p )
{
	return sqrt( p.x*p.x + p.y*p.y );
}

float sdTorus82( vec3 p, vec2 t ){
  vec2 q = vec2(length8(p.xz)-t.x,p.y);
  return length2(q)-t.y;
}


vec4 sdf_track(vec3 p){

    float t =  PI / 2;
    mat4 R = mat4(
        vec4(cos(t), sin(t), 0, 0),
        vec4(-sin(t), cos(t), 0, 0),
        vec4(0, 0, 1, 0),
        vec4(0, 0, 0, 1));

        vec3 pp = (vec4(p - vec3(0,8,0), 1) * inverse( R )).xyz;
   
    vec3 s = vec3(0,3,0);

    
    vec3 q = pp - s*round(pp/s);

    return vec4(sdTorus82( q, vec2(0.4,0.1) ),1,p.x,p.y);
}

vec4 sdf_torus0(vec3 p){
    float t = u_time * PI / 4;
    mat4 R = mat4(
        vec4(cos(t), sin(t), 0, 0),
        vec4(-sin(t), cos(t), 0, 0),
        vec4(0, 0, 1, 0),
        vec4(0, 0, 0, 1));
    vec3 pp = (vec4(p-vec3(-6,6,5), 1) * inverse( R )).xyz;

    return vec4(sdTorus(pp,vec2(2,0.2)),3,p.x,p.y);
    
    /*
    vec3 s = vec3(10,0,0);
    vec3 q = pp - s*round(pp/s);
    return vec4(sdTorus(q,vec2(2,0.2)),3,p.x,p.y);
    */
    //return vec4(sdTorus(pp,vec2(2,0.2)),3,p.x,p.y);
}

vec4 sdf_blobo(vec3 p){
    vec3 s = vec3(50,50,50);
    vec3 q = p - s*round(p/s);

    vec4 blobo0=vec4(sdBox(q-vec3(-2.2,0,0),vec3(1,1,1)), 0,0,0);
    vec4 blobo1=vec4(sdSphere(q-vec3(-2.2,0.6*sin(u_time)+1,0),0.5),4,0,0);    

    return sminsdf(blobo0,blobo1,0.4);
}

vec4 sdf(vec3 p){
    /*vec4 result=minsdf(sdf_1(p),sdf_2(p));
    result=minsdf(result,sdf_3(p));
    result=minsdf(result,sdf_4(p));
    result=minsdf(result,sdf_5(p));
    result=minsdf(result,sdf_6(p));*/
    vec4 result=minsdf(sdf_sea(p),sdf_land(p));

    result=minsdf(result,sdf_sphere0(p));
    result=minsdf(result,sdf_torus0(p));
    result=minsdf(result,sdf_track(p));
    result=minsdf(result,sdf_blobo(p));

    return result;
}

float smoothstep(float edge0, float edge1, float x) {
    // Scale, bias, and saturate x to 0..1 range
    float t = clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0);
    // Evaluate polynomial
    return t * t * (3.0 - 2.0 * t);
}

vec3 sdfcolor(vec4 hitdata){ //TODO BUMP MAP ETC?
    if (100<hitdata.x){
        return vec3(0,0,0);
    }

    int item=int(hitdata.y);
    switch(item){
        case 1:
            return texture(texwater,hitdata.ba).rgb;
        case 2:
            //return vec3(0.0,  1.0  ,0.0);
           //return texture(texbrick2,hitdata.ba).rgb;
           float height=hitdata.z;
           vec3 waterColor = vec3(0.0, 0.3, 0.5);
           vec3 sandColor = vec3(0.93, 0.87, 0.51);
           vec3 grassColor = vec3(0.2, 0.6, 0.2);
           vec3 rockColor = vec3(0.5, 0.5, 0.5);
           vec3 snowColor = vec3(1.0, 1.0, 1.0);
           if (height < 0.05) {
            return mix(waterColor, sandColor, smoothstep(0.2, 0.3, height));
           }
           if (height < 0.1) {
            return mix(sandColor, grassColor, smoothstep(0.3, 0.5, height));
           }
           if (height < 0.5) {
            return mix(grassColor, rockColor, smoothstep(0.5, 0.7, height));
           } 
           return mix(rockColor, snowColor, smoothstep(0.7, 1.0, height));
    
        case 3:
            return texture(texbrick,hitdata.ba).rgb;
            //return vec3(0.0,  0.0  ,1.0);
        case 4:
            return texture(texwater,hitdata.ba).rgb;
        case 5:
            return vec3(1.0,  0.0  ,0.0);

    }
    return vec3(1,1,1); //Default
}

//------END TOTEUTETTAVAT MODULIT------




vec4 march(vec3 ro, vec3 rd,float maxDistance) {
  float dTotal = 0.; //Distane Origin
  vec4 sdfResult;
  for(int i=0;i<MAX_STEPS;i++){
    vec3 p = ro + rd * dTotal;
    sdfResult=sdf(p);
    dTotal += sdfResult.x;
    if(dTotal > maxDistance || sdfResult.x < SURF_DIST) 
      break;
  }
  return vec4(dTotal,sdfResult.g,sdfResult.b,sdfResult.a);
}


//TODO eikun globaalit tai uniform arvot kertomaan kasa valoja

//TODO optimoi äläkä laske d uudestaan!
vec3 GetNormal(vec3 p){ 
    float d = sdf(p).x; // Distance
    vec2 e = vec2(.01,0); // Epsilon
    vec3 n = d - vec3(
    sdf(p-e.xyy).x,
    sdf(p-e.yxy).x,
    sdf(p-e.yyx).x);
 
    return normalize(n);
}


vec2 calcPointLight(vec3 p,float distance,vec3 lightPos,vec3 cameraPos){
    vec3 lightVec=lightPos-p;
    float lightDistance=length(lightVec);
    vec3 normLightVec = lightVec/lightDistance; // Light Vector
    
    //vec3 normalVec = GetNormal(p); // Normal Vector
    //Avoid re-evaluating p on normal calc
    vec3 normalVec =normalize(
        sdf(p).x-vec3(
        sdf(p-vec2(.01,0).xyy).x,
        sdf(p-vec2(.01,0).yxy).x,
        sdf(p-vec2(.01,0).yyx).x));

    // Is light hitting something?
    float d = march(p+normalVec*SURF_DIST*2., normLightVec,lightDistance).x;  //TODO miksi .2 varmuuden vuoksi?     
    if(d<lightDistance) return vec2(0,0);

    float dif = dot(normalVec,normLightVec); // Diffuse light, normal and light..
    dif = clamp(dif,0.,1.); // Clamp so it doesnt go below 0
   
    //Reflected?  R = I - 2*(N dot I)*N.
    vec3 toCamera=cameraPos-p;
    float dotprod=dot(normalVec,toCamera);
    vec3 reflected=normalize(toCamera-(2.0*dotprod)*normalVec);
    
    float beta=dot(normalize(toCamera-reflected),reflected);
    
    //beta=VEKTORIPISTETULO(vektoriNormalisoi(vektoriMiinus(kamerapaikka,lahinOsuma.sijainti)),heijastunut);

    return vec2(dif,beta);
}

//Just to one directio
vec2 calcLightDirectional(vec3 p,float distance,vec3 lightVec,vec3 cameraPos){
    float lightDistance=length(lightVec);
    vec3 normLightVec = lightVec/lightDistance; // Light Vector
    
    //vec3 normalVec = GetNormal(p); // Normal Vector
    //Avoid re-evaluating p on normal calc
    vec3 normalVec =normalize(
        sdf(p).x-vec3(
        sdf(p-vec2(.01,0).xyy).x,
        sdf(p-vec2(.01,0).yxy).x,
        sdf(p-vec2(.01,0).yyx).x));

    // Is light hitting something?
    float d = march(p+normalVec*SURF_DIST*2., normLightVec,lightDistance).x;  //TODO miksi .2 varmuuden vuoksi?     
    if(d<lightDistance) return vec2(0,0);

    float dif = dot(normalVec,normLightVec); // Diffuse light, normal and light..
    dif = clamp(dif,0.,1.); // Clamp so it doesnt go below 0
   
    //Reflected?  R = I - 2*(N dot I)*N.
    vec3 toCamera=cameraPos-p;
    float dotprod=dot(normalVec,toCamera);
    vec3 reflected=normalize(toCamera-(2.0*dotprod)*normalVec);
    
    float beta=dot(normalize(toCamera-reflected),reflected);
    
    //beta=VEKTORIPISTETULO(vektoriNormalisoi(vektoriMiinus(kamerapaikka,lahinOsuma.sijainti)),heijastunut);

    return vec2(dif,beta);
}

mat3 rotateX(float theta) {
    float c = cos(theta);
    float s = sin(theta);
    return mat3(
        vec3(1, 0, 0),
        vec3(0, c, -s),
        vec3(0, s, c)
    );
}

// Rotation matrix around the Y axis.
mat3 rotateY(float theta) {
    float c = cos(theta);
    float s = sin(theta);
    return mat3(
        vec3(c, 0, s),
        vec3(0, 1, 0),
        vec3(-s, 0, c)
    );
}

// Rotation matrix around the Z axis.
mat3 rotateZ(float theta) {
    float c = cos(theta);
    float s = sin(theta);
    return mat3(
        vec3(c, -s, 0),
        vec3(s, c, 0),
        vec3(0, 0, 1)
    );
}

//Main function for calling all... recrusing reflection?
void main(){
    vec2 uv = (gl_FragCoord.xy-.5*resolution.xy)/resolution.y;

    vec3 ro = player; //vec3(0,1,0); // Ray Origin/Camera
    vec3 rd = normalize(vec3(uv.x,uv.y,1))*rotateX(theta) *rotateY(phi) ; // Ray Direction

    vec4 hitdata=march(ro,rd,MAX_DIST);
    vec3 hitPoint = ro + rd * hitdata.x; //Arvio osumapaikasta

    /*moving point light
    vec3 lightPos = vec3(5.*sin(u_time),5.,5.0*cos(u_time)); // Light Position
    vec2 lightModelCoeff=calcPointLight(hitPoint,hitdata.x,lightPos,ro); // Diffuse lighting
    */
    vec2 lightModelCoeff=calcLightDirectional(hitPoint,hitdata.x,vec3(3,0,-1),ro);

    vec3 color = sdfcolor(hitdata);
    
    lightModelCoeff.x+=0.4;

    gl_FragColor=vec4(color.rgb*(lightModelCoeff.x),1.0); //Pelkkä dihvuusimalli?
}
