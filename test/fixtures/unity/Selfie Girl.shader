Shader "ShaderConverter/Selfie Girl"
	{

	Properties{
	_Volume1Tex ("Volume1Tex", 3D) = "white" {}
_SecondTex ("SecondTex", 2D) = "white" {}
_ThirdTex ("ThirdTex", 2D) = "white" {}
_FourthTex ("FourthTex", 2D) = "white" {}

	}

	SubShader
	{
	Tags { "RenderType" = "Transparent" "Queue" = "Transparent" }

	Pass
	{
	ZWrite Off
	Blend SrcAlpha OneMinusSrcAlpha

	CGPROGRAM
	#pragma vertex vert
	#pragma fragment frag
	#include "UnityCG.cginc"

	struct VertexInput {
		float4 vertex : POSITION;
		float2 uv:TEXCOORD0;
		float4 tangent : TANGENT;
		float3 normal : NORMAL;
	};


	struct VertexOutput {
	float4 pos : SV_POSITION;
	float2 uv:TEXCOORD0;
	};

	sampler3D _Volume1Tex;
sampler2D _SecondTex;
sampler2D _ThirdTex;
sampler2D _FourthTex;


	
        // Basic utility functions (SDFs, noises, shaping functions)
// and also the camera setup which is shared between the
// background rendering code ("Buffer A" tab) and the character
// rendering code ("Image" tab)



// https://iquilezles.org/articles/smin
fixed smin( fixed a, fixed b, fixed k )
{
    fixed h = max(k-abs(a-b),0.0);
    return min(a, b) - h*h*0.25/k;
}

// https://iquilezles.org/articles/smin
fixed smax( fixed a, fixed b, fixed k )
{
    k *= 1.4;
    fixed h = max(k-abs(a-b),0.0);
    return max(a, b) + h*h*h/(6.0*k*k);
}

// https://iquilezles.org/articles/smin
fixed smin3( fixed a, fixed b, fixed k )
{
    k *= 1.4;
    fixed h = max(k-abs(a-b),0.0);
    return min(a, b) - h*h*h/(6.0*k*k);
}

// https://iquilezles.org/articles/smin
fixed sclamp(in fixed x, in fixed a, in fixed b )
{
    fixed k = 0.1;
	return smax(smin(x,b,k),a,k);
}

// https://iquilezles.org/articles/functions/
fixed sabs(in fixed x, in fixed k )
{
    return sqrt(x*x+k);
}

// https://iquilezles.org/articles/distfunctions
fixed opOnion( in fixed sdf, in fixed thickness )
{
    return abs(sdf)-thickness;
}

// https://iquilezles.org/articles/distfunctions
fixed opRepLim( in fixed p, in fixed s, in fixed lima, in fixed limb )
{
    return p-s*clamp(round(p/s),lima,limb);
}

fixed det( fixed2 a, fixed2 b ) { return a.x*b.y-b.x*a.y; }
fixed ndot(fixed2 a, fixed2 b ) { return a.x*b.x-a.y*b.y; }
fixed dot2( in fixed2 v ) { return dot(v,v); }
fixed dot2( in fixed3 v ) { return dot(v,v); }

// https://iquilezles.org/articles/distfunctions
fixed sdTorus( in fixed3 p, in fixed ra, in fixed rb )
{
    return length( fixed2(length(p.xz)-ra,p.y) )-rb;
}

// https://iquilezles.org/articles/distfunctions
fixed sdCappedTorus(in fixed3 p, in fixed2 sc, in fixed ra, in fixed rb)
{
    p.x = abs(p.x);
    fixed k = (sc.y*p.x>sc.x*p.z) ? dot(p.xz,sc) : length(p.xz);
    return sqrt( dot(p,p) + ra*ra - 2.0*ra*k ) - rb;
}

// https://iquilezles.org/articles/distfunctions
fixed sdSphere( in fixed3 p, in fixed r ) 
{
    return length(p)-r;
}

// https://iquilezles.org/articles/distfunctions
fixed sdEllipsoid( in fixed3 p, in fixed3 r ) 
{
    fixed k0 = length(p/r);
    fixed k1 = length(p/(r*r));
    return k0*(k0-1.0)/k1;
}

// https://iquilezles.org/articles/distfunctions
fixed sdBox( in fixed3 p, in fixed3 b )
{
    fixed3 d = abs(p) - b;
    return min( max(max(d.x,d.y),d.z),0.0) + length(max(d,0.0));
}

// https://iquilezles.org/articles/distfunctions
fixed sdArc( in fixed2 p, in fixed2 scb, in fixed ra )
{
    p.x = abs(p.x);
    fixed k = (scb.y*p.x>scb.x*p.y) ? dot(p.xy,scb) : length(p.xy);
    return sqrt( dot(p,p) + ra*ra - 2.0*ra*k );
}

#if 1
// http://research.microsoft.com/en-us/um/people/hoppe/ravg.pdf
// { dist, t, y (above the plane of the curve, x (away from curve in the plane of the curve))
fixed4 sdBezier( fixed3 p, fixed3 va, fixed3 vb, fixed3 vc )
{
  fixed3 w = normalize( cross( vc-vb, va-vb ) );
  fixed3 u = normalize( vc-vb );
  fixed3 v =          ( cross( w, u ) );
  //----  
  fixed2 m = fixed2( dot(va-vb,u), dot(va-vb,v) );
  fixed2 n = fixed2( dot(vc-vb,u), dot(vc-vb,v) );
  fixed3 q = fixed3( dot( p-vb,u), dot( p-vb,v), dot(p-vb,w) );
  //----  
  fixed mn = det(m,n);
  fixed mq = det(m,q.xy);
  fixed nq = det(n,q.xy);
  //----  
  fixed2  g = (nq+mq+mn)*n + (nq+mq-mn)*m;
  fixed f = (nq-mq+mn)*(nq-mq+mn) + 4.0*mq*nq;
  fixed2  z = 0.5*f*fixed2(-g.y,g.x)/dot(g,g);
//float t = clamp(0.5+0.5*(det(z,m+n)+mq+nq)/mn, 0.0 ,1.0 );
  fixed t = clamp(0.5+0.5*(det(z-q.xy,m+n))/mn, 0.0 ,1.0 );
  fixed2 cp = m*(1.0-t)*(1.0-t) + n*t*t - q.xy;
  //----  
  fixed d2 = dot(cp,cp);
  return fixed4(sqrt(d2+q.z*q.z), t, q.z, -sign(f)*sqrt(d2) );
}
#else
fixed det( fixed3 a, fixed3 b, in fixed3 v ) { return dot(v,cross(a,b)); }

// my adaptation to 3d of http://research.microsoft.com/en-us/um/people/hoppe/ravg.pdf
// { dist, t, y (above the plane of the curve, x (away from curve in the plane of the curve))
fixed4 sdBezier( fixed3 p, fixed3 b0, fixed3 b1, fixed3 b2 )
{
    b0 -= p;
    b1 -= p;
    b2 -= p;
    
    fixed3  d21 = b2-b1;
    fixed3  d10 = b1-b0;
    fixed3  d20 = (b2-b0)*0.5;

    fixed3  n = normalize(cross(d10,d21));

    fixed a = det(b0,b2,n);
    fixed b = det(b1,b0,n);
    fixed d = det(b2,b1,n);
    fixed3  g = b*d21 + d*d10 + a*d20;
	fixed f = a*a*0.25-b*d;

    fixed3  z = cross(b0,n) + f*g/dot(g,g);
    fixed t = clamp( dot(z,d10-d20)/(a+b+d), 0.0 ,1.0 );
    fixed3 q = lerp(lerp(b0,b1,t), lerp(b1,b2,t),t);
    
    fixed k = dot(q,n);
    return fixed4(length(q),t,-k,-sign(f)*length(q-n*k));
}
#endif

// https://iquilezles.org/articles/distfunctions
fixed2 sdSegment(fixed3 p, fixed3 a, fixed3 b)
{
    fixed3 pa = p-a, ba = b-a;
	fixed h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
	return fixed2( length( pa - ba*h ), h );
}

// https://iquilezles.org/articles/distfunctions
fixed2 sdSegmentOri(fixed2 p, fixed2 b)
{
	fixed h = clamp( dot(p,b)/dot(b,b), 0.0, 1.0 );
	return fixed2( length( p - b*h ), h );
}

// https://iquilezles.org/articles/distfunctions
fixed sdFakeRoundCone(fixed3 p, fixed b, fixed r1, fixed r2)
{
    fixed h = clamp( p.y/b, 0.0, 1.0 );
    p.y -= b*h;
	return length(p) - lerp(r1,r2,h);
}

// https://iquilezles.org/articles/distfunctions
fixed sdCone( in fixed3 p, in fixed2 c )
{
  fixed2 q = fixed2( length(p.xz), p.y );

  fixed2 a = q - c*clamp( (q.x*c.x+q.y*c.y)/dot(c,c), 0.0, 1.0 );
  fixed2 b = q - c*fixed2( clamp( q.x/c.x, 0.0, 1.0 ), 1.0 );
  
  fixed s = -sign( c.y );
  fixed2 d = min( fixed2( dot( a, a ), s*(q.x*c.y-q.y*c.x) ),
			    fixed2( dot( b, b ), s*(q.y-c.y)  ));
  return -sqrt(d.x)*sign(d.y);
}

// https://iquilezles.org/articles/distfunctions
fixed sdRhombus(fixed3 p, fixed la, fixed lb, fixed h, fixed ra)
{
    p = abs(p);
    fixed2 b = fixed2(la,lb);
    fixed f = clamp( (ndot(b,b-2.0*p.xz))/dot(b,b), -1.0, 1.0 );
	fixed2 q = fixed2(length(p.xz-0.5*b*fixed2(1.0-f,1.0+f))*sign(p.x*b.y+p.z*b.x-b.x*b.y)-ra, p.y-h);
    return min(max(q.x,q.y),0.0) + length(max(q,0.0));
}

// https://iquilezles.org/articles/distfunctions
fixed4 opElongate( in fixed3 p, in fixed3 h )
{
    fixed3 q = abs(p)-h;
    return fixed4( max(q,0.0), min(max(q.x,max(q.y,q.z)),0.0) );
}

//-----------------------------------------------

// ray-infinite-cylinder intersection
fixed2 iCylinderY( in fixed3 ro, in fixed3 rd, in fixed rad )
{
	fixed3 oc = ro;
    fixed a = dot( rd.xz, rd.xz );
	fixed b = dot( oc.xz, rd.xz );
	fixed c = dot( oc.xz, oc.xz ) - rad*rad;
	fixed h = b*b - a*c;
	if( h<0.0 ) return fixed2(-1.0,-1.0);
    h = sqrt(h);
	return fixed2(-b-h,-b+h)/a;
}

// ray-infinite-cone intersection
fixed2 iConeY(in fixed3 ro, in fixed3 rd, in fixed k )
{
	fixed a = dot(rd.xz,rd.xz) - k*rd.y*rd.y;
    fixed b = dot(ro.xz,rd.xz) - k*ro.y*rd.y;
    fixed c = dot(ro.xz,ro.xz) - k*ro.y*ro.y; 
        
    fixed h = b*b-a*c;
    if( h<0.0 ) return fixed2(-1.0,-1.0);
    h = sqrt(h);
    return fixed2(-b-h,-b+h)/a;
}

//-----------------------------------------------

fixed linearstep(fixed a, fixed b, in fixed x )
{
    return clamp( (x-a)/(b-a), 0.0, 1.0 );
}

fixed2 rot( in fixed2 p, in fixed an )
{
    fixed cc = cos(an);
    fixed ss = sin(an);
    return fixed2x2(cc,-ss,ss,cc)*p;
}

fixed expSustainedImpulse( fixed t, fixed f, fixed k )
{
    return smoothstep(0.0,f,t)*1.1 - 0.1*exp2(-k*max(t-f,0.0));
}

//-----------------------------------------------

fixed3 hash3( uint n ) 
{
    // integer hash copied from Hugo Elias
	n = (n << 13U) ^ n;
    n = n * (n * n * 15731U + 789221U) + 1376312589U;
    ufixed3 k = n * ufixed3(n,n*16807U,n*48271U);
    return fixed3( k & ufixed3(0x7fffffffU,0x7fffffffU,0x7fffffffU))/fixed(0x7fffffff);
}

//---------------------------------------

fixed noise1( sampler3D tex, in fixed3 x )
{
    return tex2Dlod(tex,float4((x+0.5)/32.0,0.0,0)).x;
}
fixed noise1( sampler2D tex, in fixed2 x )
{
    return tex2Dlod(tex,float4((x+0.5)/64.0,0.0,0)).x;
}
fixed noise1f( sampler2D tex, in fixed2 x )
{
    return tex2D(tex,(x+0.5)/64.0).x;
}
fixed fbm1( sampler3D tex, in fixed3 x )
{
    fixed f = 0.0;
    f += 0.5000*noise1(tex,x); x*=2.01;
    f += 0.2500*noise1(tex,x); x*=2.01;
    f += 0.1250*noise1(tex,x); x*=2.01;
    f += 0.0625*noise1(tex,x);
    f = 2.0*f-0.9375;
    return f;
}

fixed fbm1( sampler2D tex, in fixed2 x )
{
    fixed f = 0.0;
    f += 0.5000*noise1(tex,x); x*=2.01;
    f += 0.2500*noise1(tex,x); x*=2.01;
    f += 0.1250*noise1(tex,x); x*=2.01;
    f += 0.0625*noise1(tex,x);
    f = 2.0*f-0.9375;
    return f;
}
fixed fbm1f( sampler2D tex, in fixed2 x )
{
    fixed f = 0.0;
    f += 0.5000*noise1f(tex,x); x*=2.01;
    f += 0.2500*noise1f(tex,x); x*=2.01;
    f += 0.1250*noise1f(tex,x); x*=2.01;
    f += 0.0625*noise1f(tex,x);
    f = 2.0*f-0.9375;
    return f;
}
fixed bnoise( in fixed x )
{
    fixed i = floor(x);
    fixed f = frac(x);
    fixed s = sign(frac(x/2.0)-0.5);
    fixed k = 0.5+0.5*sin(i);
    return s*f*(f-1.0)*((16.0*k-4.0)*f*(f-1.0)-1.0);
}
fixed3 fbm13( in fixed x, in fixed g )
{    
    fixed3 n = fixed3(0.0,0.0,0.0);
    fixed s = 1.0;
    for( int i=0; i<6; i++ )
    {
        n += s*fixed3(bnoise(x),bnoise(x+13.314),bnoise(x+31.7211));
        s *= g;
        x *= 2.01;
        x += 0.131;
    }
    return n;
}

//--------------------------------------------------
//const float X1 = 1.6180339887498948; const float H1 = float( 1.0/X1 );
//const float X2 = 1.3247179572447460; const vec2  H2 = vec2(  1.0/X2, 1.0/(X2*X2) );
//const float X3 = 1.2207440846057595; const vec3  H3 = vec3(  1.0/X3, 1.0/(X3*X3), 1.0/(X3*X3*X3) );
  const fixed X4 = 1.1673039782614187; const fixed4  H4 = fixed4(  1.0/X4, 1.0/(X4*X4), 1.0/(X4*X4*X4), 1.0/(X4*X4*X4*X4) );

//--------------------------------------
fixed3x3 calcCamera( in fixed time, out fixed3 oRo, out fixed oFl )
{
    fixed3 ta = fixed3( 0.0, -0.3, 0.0 );
    fixed3 ro = fixed3( -0.5563, -0.2, 2.7442 );
    fixed fl = 1.7;
#if 0
    fixed3 fb = fbm13( 0.2*time, 0.5 );
    ta += 0.025*fb;
    fixed cr = -0.01 + 0.006*fb.z;
#else
    fixed3 fb1 = fbm13( 0.15*time, 0.50 );
    ro.xyz += 0.010*fb1.xyz;
    fixed3 fb2 = fbm13( 0.33*time, 0.65 );
    fb2 = fb2*fb2*sign(fb2);
    ta.xy += 0.005*fb2.xy;
    fixed cr = -0.01 + 0.002*fb2.z;
#endif
    
    // camera matrix
    fixed3 ww = normalize( ta - ro );
    fixed3 uu = normalize( cross(ww,fixed3(sin(cr),cos(cr),0.0) ) );
    fixed3 vv =          ( cross(uu,ww));
    
    oRo = ro;
    oFl = fl;

    return fixed3x3(uu,vv,ww);
}

#define ZERO min(iFrame,0)
#define ZEROU min(uint(iFrame),0u)
// Copyright Inigo Quilez, 2020 - https://iquilezles.org/
// I am the sole copyright owner of this Work.
// You cannot host, display, distribute or share this Work neither
// as it is or altered, here on Shadertoy or anywhere else, in any
// form including physical and digital. You cannot use this Work in any
// commercial or non-commercial product, website or project. You cannot
// sell this Work and you cannot mint an NFTs of it or train a neural
// network with it without permission. I share this Work for educational
// purposes, and you can link to it, through an URL, proper attribution
// and unmodified screenshot, as part of your educational material. If
// these conditions are too restrictive please contact me and we'll
// definitely work it out.


// Source code of the mathematical painting "Selfie Girl".
// Making-of video on Youtube:
//
// Tutorial on Youtube:  https://www.youtube.com/watch?v=8--5LwHRhjk
// Tutorial on Bilibili: https://www.bilibili.com/video/BV1Hu4y1y7U1

// The image is a single formula, but I had to split it
// down into 3 passes here so it could be shared without
// breaking the WebGL implementation of the web browsers
// (which is what Shadertoy uses to run the code below
// that implements the formula).

// This "Image" tab in particular renders the girl through
// raymarching and then performs the final composition with
// the background, which is computed in "Buffer B" (open
// the rest of the tabs to see explanations of what each
// one does). For the rendering/computer graphics people - 
// there's no TAA in this pass because I didn't want to
// compute velocity vectors for the animation, so things
// alias a bit (feel free to change the AA define below to
// 2 if you have a fast GPU)

#define AA 1


// This SDF is really 6 braids at once (through domain
// repetition) with three strands each (brute forced)
fixed4 sdHair( fixed3 p, fixed3 pa, fixed3 pb, fixed3 pc, fixed an, out fixed2 occ_id) 
{
    fixed4 b = sdBezier(p, pa,pb,pc );
    fixed2 q = rot(b.zw,an);
  	
    fixed2 id2 = round(q/0.1);
    id2 = clamp(id2,fixed2(0,0),fixed2(2,1));
    q -= 0.1*id2;

    fixed id = 11.0*id2.x + id2.y*13.0;

    q += smoothstep(0.5,0.8,b.y)*0.02*fixed2(0.4,1.5)*cos( 23.0*b.y + id*fixed2(13,17));

    occ_id.x = clamp(length(q)*8.0-0.2,0.0,1.0);
    fixed4 res = fixed4(99,q,b.y);
    for( int i=0; i<3; i++ )
    {
        fixed2 tmp = q + 0.01*cos( id + 180.0*b.y + fixed2(2*i,6-2*i));
        fixed lt = length(tmp)-0.02;
        if( lt<res.x )
        { 
            occ_id.y = id+fixed(i); 
            res.x = lt; 
            res.yz = tmp;
        }
    }
    return res;
}

// The SDF for the hoodie and jacket. It's a very distorted
// ellipsoid, torus section, a segment and a sphere.
fixed4 sdHoodie( in fixed3 pos )
{
    fixed3 opos = pos;

    pos.x   += 0.09*sin(3.5*pos.y-0.5)*sin(    pos.z) + 0.015;
    pos.xyz += 0.03*sin(2.0*pos.y)*sin(7.0*pos.zyx);
    
    // hoodie
    fixed3 hos = pos-fixed3(0.0,-0.33,0.15);
    hos.x -= 0.031*smoothstep(0.0,1.0,opos.y+0.33);
    hos.yz = rot(hos.yz,0.9);
    fixed d1 = sdEllipsoid(hos,fixed3(0.96-pos.y*0.1,1.23,1.5));
	fixed d2 = 0.95*pos.z-0.312*pos.y-0.9;
    fixed d = max(opOnion(d1,0.01), d2 );
    
    // shoulders
    fixed3 sos = fixed3( abs(pos.x), pos.yz );    
    fixed2 se = sdSegment(sos, fixed3(0.18,-1.6,-0.3), fixed3(1.1,-1.9,0.0) );
    d = smin(d,se.x-lerp(0.25,0.43,se.y),0.4);
    d = smin(d,sdSphere(sos-fixed3(0.3,-2.2,0.4), 0.5 ),0.2);

    // neck
    opos.x -= 0.02*sin(9.0*opos.y);
    fixed4 w = opElongate( opos-fixed3(0.0,-1.2,0.3), fixed3(0.0,0.3,0.0) );
    d = smin(d,
             w.w+sdCappedTorus(fixed3(w.xy,-w.z),fixed2(0.6,-0.8),0.6,0.02),
             0.1);
    
    // bumps
    d += 0.004*sin(pos.x*90.0)*sin(pos.y*90.0)*sin(pos.z*90.0);
    d -= 0.002*sin(pos.x*300.0);
    d -= 0.02*(1.0-smoothstep(0.0,0.04,abs(opOnion(pos.x,1.1))));
    
    // border
    d = min(d,length(fixed2(d1,d2))-0.015);
    
    return fixed4(d,pos);
}

// moves the head (and hair and hoodie). This could be done
// more efficiently (with a single matrix or quaternion),
// but this code was optimized for editing, not for runtime
fixed3 moveHead( in fixed3 pos, in fixed3 an, in fixed amount)
{
    pos.y -= -1.0;
    pos.xz = rot(pos.xz,amount*an.x);
    pos.xy = rot(pos.xy,amount*an.y);
    pos.yz = rot(pos.yz,amount*an.z);
    pos.y += -1.0;
    return pos;
}

// the animation state
fixed3 animData; // { blink, nose follow up, mouth } 
fixed3 animHead; // { head rotation angles }

// SDF of the girl. It is not as efficient as it should, 
// both in terms of performance and euclideanness of the
// returned distance. Among other things I tweaked the
// overal shape of the head though scaling right in the
// middle of the design process (see 1.02 and 1.04 numbers
// below). I should have backpropagated those adjustements
// to the  primitives themselves, but I didn't and now it's
// too late. So, I am paying some cost there.
//
// She is modeled to camera (her face's shape looks bad
// from other perspectives. She's made of five ellipsoids
// blended together for the face, a cone and three spheres
// for the nose, a torus for the teeh and two quadratic 
// curves for the lips. The neck is a cylinder, the hair
// is made of three quadratic curves that are repeated
// multiple times through domain repetition and each of
// them contains three more curves in order to make the
// braids. The hoodie is an ellipsoid deformed with
// two sine waves and cut in half, the neck is an elongated
// torus section and the shoulders are capsules.
//
fixed4 map( in fixed3 pos, in fixed time, out fixed outMat, out fixed3 uvw )
{
    outMat = 1.0;

    fixed3 oriPos = pos;
    
    // head deformation and transformation
    pos.y /= 1.04;
    fixed3 opos;
    opos = moveHead( pos, animHead, smoothstep(-1.2, 0.2,pos.y) );
    pos  = moveHead( pos, animHead, smoothstep(-1.4,-1.0,pos.y) );
    pos.x *= 1.04;
    pos.y /= 1.02;
    uvw = pos;

    // symmetric coord systems (sharp, and smooth)
    fixed3 qos = fixed3(abs(pos.x),pos.yz);
    fixed3 sos = fixed3(sabs(qos.x,0.005),pos.yz);
    
    // head
    fixed d = sdEllipsoid( pos-fixed3(0.0,0.05,0.07), fixed3(0.8,0.75,0.85) );
    
    // jaw
    fixed3 mos = pos-fixed3(0.0,-0.38,0.35); mos.yz = rot(mos.yz,0.4);
	mos.yz = rot(mos.yz,0.1*animData.z);
	fixed d2 = sdEllipsoid(mos-fixed3(0,-0.17,0.16),
                 fixed3(0.66+sclamp(mos.y*0.9-0.1*mos.z,-0.3,0.4),
                 	  0.43+sclamp(mos.y*0.5,-0.5,0.2),
                      0.50+sclamp(mos.y*0.3,-0.45,0.5)));
        
    // mouth hole
    d2 = smax(d2,-sdEllipsoid(mos-fixed3(0,0.06,0.6+0.05*animData.z), fixed3(0.16,0.035+0.05*animData.z,0.1)),0.01);
    
    // lower lip    
    fixed4 b = sdBezier(fixed3(abs(mos.x),mos.yz), 
                      fixed3(0,0.01,0.61),
                      fixed3(0.094+0.01*animData.z,0.015,0.61),
                      fixed3(0.18-0.02*animData.z,0.06+animData.z*0.05,0.57-0.006*animData.z));
    fixed isLip = smoothstep(0.045,0.04,b.x+b.y*0.03);
    d2 = smin(d2,b.x - 0.027*(1.0-b.y*b.y)*smoothstep(1.0,0.4,b.y),0.02);
    d = smin(d,d2,0.19);

    // chicks
    d = smin(d,sdSphere(qos-fixed3(0.2,-0.33,0.62),0.28 ),0.04);
    
    // who needs ears
    
    // eye sockets
    fixed3 eos = sos-fixed3(0.3,-0.04,0.7);
    eos.xz = rot(eos.xz,-0.2);
    eos.xy = rot(eos.xy,0.3);
    eos.yz = rot(eos.yz,-0.2);
    d2 = sdEllipsoid( eos-fixed3(-0.05,-0.05,0.2), fixed3(0.20,0.14-0.06*animData.x,0.1) );
	d = smax( d, -d2, 0.15 );

    eos = sos-fixed3(0.32,-0.08,0.8);
    eos.xz = rot(eos.xz,-0.4);
    d2 = sdEllipsoid( eos, fixed3(0.154,0.11,0.1) );
    d = smax( d, -d2, 0.05 );

    fixed3 oos = qos - fixed3(0.25,-0.06,0.42);
    
    // eyelid
    d2 = sdSphere( oos, 0.4 );
    oos.xz = rot(oos.xz, -0.2);
    oos.xy = rot(oos.xy, 0.2);
    fixed3 tos = oos;        
    oos.yz = rot(oos.yz,-0.6+0.58*animData.x);

    //eyebags
    tos = tos-fixed3(-0.02,0.06,0.2+0.02*animData.x);
    tos.yz = rot(tos.yz,0.8);
    tos.xy = rot(tos.xy,-0.2);
	d = smin( d, sdTorus(tos,0.29,0.01), 0.03 );
    
    // eyelids
    eos = qos - fixed3(0.33,-0.07,0.53);
    eos.xy = rot(eos.xy, 0.2);
    eos.yz = rot(eos.yz,0.35-0.25*animData.x);
    d2 = smax(d2-0.005, -max(oos.y+0.098,-eos.y-0.025), 0.02 );
    d = smin( d, d2, 0.012 );

	// eyelashes
	oos.x -= 0.01;
    fixed xx = clamp( oos.x+0.17,0.0,1.0);
    fixed ra = 0.35 + 0.1*sqrt(xx/0.2)*(1.0-smoothstep(0.3,0.4,xx))*(0.6+0.4*sin(xx*256.0));
    fixed rc = 0.18/(1.0-0.7*smoothstep(0.15,0.5,animData.x));
    oos.y -= -0.18 - (rc-0.18)/1.8;
    d2 = (1.0/1.8)*sdArc( oos.xy*fixed2(1.0,1.8), fixed2(0.9,sqrt(1.0-0.9*0.9)), rc )-0.001;
    fixed deyelashes = max(d2,length(oos.xz)-ra)-0.003;
    
    // nose
    eos = pos-fixed3(0.0,-0.079+animData.y*0.005,0.86);
    eos.yz = rot(eos.yz,-0.23);
    fixed h = smoothstep(0.0,0.26,-eos.y);
    d2 = sdCone( eos-fixed3(0.0,-0.02,0.0), fixed2(0.03,-0.25) )-0.04*h-0.01;
    eos.x = sqrt(eos.x*eos.x + 0.001);
    d2 = smin( d2, sdSphere(eos-fixed3(0.0, -0.25,0.037),0.06 ), 0.07 );
    d2 = smin( d2, sdSphere(eos-fixed3(0.1, -0.27,0.03 ),0.04 ), 0.07 );
    d2 = smin( d2, sdSphere(eos-fixed3(0.0, -0.32,0.05 ),0.025), 0.04 );        
    d2 = smax( d2,-sdSphere(eos-fixed3(0.07,-0.31,0.038),0.02 ), 0.035 );
    d = smin(d,d2,0.05-0.03*h);
    
    // mouth
    eos = pos-fixed3(0.0,-0.38+animData.y*0.003+0.01*animData.z,0.71);
    tos = eos-fixed3(0.0,-0.13,0.06);
    tos.yz = rot(tos.yz,0.2);
    fixed dTeeth = sdTorus(tos,0.15,0.015);
    eos.yz = rot(eos.yz,-0.5);
    eos.x /= 1.04;

    // nose-to-upperlip connection
    d2 = sdCone( eos-fixed3(0,0,0.03), fixed2(0.14,-0.2) )-0.03;
    d2 = max(d2,-(eos.z-0.03));
    d = smin(d,d2,0.05);

    // upper lip
    eos.x = abs(eos.x);
    b = sdBezier(eos, fixed3(0.00,-0.22,0.17),
                      fixed3(0.08,-0.22,0.17),
                      fixed3(0.17-0.02*animData.z,-0.24-0.01*animData.z,0.08));
    d2 = length(b.zw/fixed2(0.5,1.0)) - 0.03*clamp(1.0-b.y*b.y,0.0,1.0);
    d = smin(d,d2,0.02);
    isLip = max(isLip,(smoothstep(0.03,0.005,abs(b.z+0.015+abs(eos.x)*0.04))
                 -smoothstep(0.45,0.47,eos.x-eos.y*1.15)));

    // valley under nose
    fixed2 se = sdSegment(pos, fixed3(0.0,-0.45,1.01),  fixed3(0.0,-0.47,1.09) );
    d2 = se.x-0.03-0.06*se.y;
    d = smax(d,-d2,0.04);
    isLip *= smoothstep(0.01,0.03,d2);

    // neck
    se = sdSegment(pos, fixed3(0.0,-0.65,0.0), fixed3(0.0,-1.7,-0.1) );
    d2 = se.x - 0.38;

    // shoulders
    se = sdSegment(sos, fixed3(0.0,-1.55,0.0), fixed3(0.6,-1.65,0.0) );
    d2 = smin(d2,se.x-0.21,0.1);
    d = smin(d,d2,0.4);
        
    // register eyelases now
    fixed4 res = fixed4( d, isLip, 0, 0 );
    if( deyelashes<res.x )
    {
        res.x = deyelashes*0.8;
        res.yzw = fixed3(0.0,1.0,0.0);
    }
    // register teeth now
    if( dTeeth<res.x )
    {
        res.x = dTeeth;
        outMat = 5.0;
    }
 
    // eyes
	pos.x /=1.05;        
    eos = qos-fixed3(0.25,-0.06,0.42);
    d2 = sdSphere(eos,0.4);
    if( d2<res.x ) 
    { 
        res.x = d2;
     	outMat = 2.0;
        uvw = pos;
    }
        
    // hair
    {
        fixed2 occ_id, tmp;
		qos = pos; qos.x=abs(pos.x);

        fixed4 pres = sdHair(pos,fixed3(-0.3, 0.55,0.8), 
                               fixed3( 0.95, 0.7,0.85), 
                               fixed3( 0.4,-1.45,0.95),
                               -0.9,occ_id);

        fixed4 pres2 = sdHair(pos,fixed3(-0.4, 0.6,0.55), 
                                fixed3(-1.0, 0.4,0.2), 
                                fixed3(-0.6,-1.4,0.7),
                                0.6,tmp);
        if( pres2.x<pres.x ) { pres=pres2; occ_id=tmp;  occ_id.y+=40.0;}

        pres2 = sdHair(qos,fixed3( 0.4, 0.7,0.4), 
                           fixed3( 1.0, 0.5,0.45), 
                           fixed3( 0.4,-1.45,0.55),
                           -0.2,tmp);
        if( pres2.x<pres.x ) { pres=pres2; occ_id=tmp;  occ_id.y+=80.0;}
    

        pres.x *= 0.8;
        if( pres.x<res.x )
        {
            res = fixed4( pres.x, occ_id.y, 0.0, occ_id.x );
            uvw = pres.yzw;
            outMat = 4.0;
        }
    }

    // hoodie
    fixed4 tmp = sdHoodie( opos );
    if( tmp.x<res.x )
    {
        res.x = tmp.x;
        outMat = 3.0;
        uvw  = tmp.yzw;
    }

    return res;
}

// SDF of the girl again, but with extra high frequency
// modeling detail. While the previous one is used for
// raymarching and shadowing, this one is used for normal
// computation. This separation is conceptually equivalent
// to decoupling detail from base geometry with "normal
// maps", but done in 3D and with SDFs, which is way
// simpler and can be done corretly (something rarely seen
// in 3D engines) without any complexity.
fixed4 mapD( in fixed3 pos, in fixed time )
{
    fixed matID;
    fixed3 uvw;
    fixed4 h = map(pos, time, matID, uvw);
    
    if( matID<1.5 ) // skin
    {
        // pores
        fixed d = fbm1(_Volume1Tex,120.0*uvw);
        h.x += 0.0015*d*d;
    }
    else if( matID>3.5 && matID<4.5 ) // hair
    {
        // some random displacement to evoke hairs
        fixed te = tex2Dlod( _ThirdTex,fixed4( 0.25*atan(uvw.x,uvw.y,fixed2(8.0*uvw.z) ,8.0*uvw.z) ))).x;
    	h.x -= 0.02*te;
    }    
    return h;
}

// Computes the normal of the girl's surface (the gradient
// of the SDF). The implementation is weird because of the
// technicalities of the WebGL API that forces us to do
// some trick to prevent code unrolling. More info here:
//
// https://iquilezles.org/articles/normalsSDF
//
fixed3 calcNormal( in fixed3 pos, in fixed time )
{
    const fixed eps = 0.001;
#if 0    
    fixed2 e = fixed2(1.0,-1.0)*0.5773;
    return normalize( e.xyy*map( pos + e.xyy*eps,time,kk ).x + 
					  e.yyx*map( pos + e.yyx*eps,time,kk ).x + 
					  e.yxy*map( pos + e.yxy*eps,time,kk ).x + 
					  e.xxx*map( pos + e.xxx*eps,time,kk ).x );
#else
    fixed4 n = fixed4(0.0,0.0,0.0,0.0);
    for( int i=ZERO; i<4; i++ )
    {
        fixed4 s = fixed4(pos, 0.0);
        fixed kk; fixed3 kk2;
        s[i] += eps;
        n[i] = mapD(s.xyz, time).x;
      //if( n.x+n.y+n.z+n.w>100.0 ) break;
    }
    return normalize(n.xyz-n.w);
#endif   
}

// Compute soft shadows for a given light, with a single
// ray insead of using montecarlo integration or shadowmap
// blurring. More info here:
//
// https://iquilezles.org/articles/rmshadows
//
fixed calcSoftshadow( in fixed3 ro, in fixed3 rd, in fixed mint, in fixed tmax, in fixed time, fixed k )
{
    // first things first - let's do a bounding volume test
    fixed2 sph = iCylinderY( ro, rd, 1.5 );
  //vec2 sph = iConeY(ro-vec3(-0.05,3.7,0.35),rd,0.08);
    tmax = min(tmax,sph.y);

    // raymarch and track penumbra    
    fixed res = 1.0;
    fixed t = mint;
    for( int i=0; i<128; i++ )
    {
        fixed kk; fixed3 kk2;
		fixed h = map( ro + rd*t, time, kk, kk2 ).x;
        res = min( res, k*h/t );
        t += clamp( h, 0.005, 0.1 );
        if( res<0.002 || t>tmax ) break;
    }
    return max( res, 0.0 );
}

// Computes convexity for our girl SDF, which can be used
// to approximate ambient occlusion. More info here:
//
// https://iquilezles.org/www/material/nvscene2008/rwwtt.pdf
//
fixed calcOcclusion( in fixed3 pos, in fixed3 nor, in fixed time )
{
    fixed kk; fixed3 kk2;
	fixed ao = 0.0;
    fixed off = tex2Dlod(_FourthTex,((i.screenCoord.xy/i.screenCoord.w)*_ScreenParams.xy).xy/256.0,0.0).x;
    fixed4 k = fixed4(0.7012912,0.3941462,0.8294585,0.109841)+off;
    for( int i=ZERO; i<16; i++ )
    {
		k = frac(k + H4);
        fixed3 ap = normalize(-1.0+2.0*k.xyz);
        fixed h = k.w*0.1;
        ap = (nor+ap)*h;
        fixed d = map( pos+ap, time, kk, kk2 ).x;
        ao += max(0.0,h-d);
        if( ao>16.0 ) break;
    }
	ao /= 16.0;
    return clamp( 1.0-ao*24.0, 0.0, 1.0 );
}

// Computes the intersection point between our girl SDF and
// a ray (coming form the camera in this case). It's a
// traditional and basic/uncomplicated SDF raymarcher. More
// info here:
//
// https://iquilezles.org/www/material/nvscene2008/rwwtt.pdf
//
fixed2 intersect( in fixed3 ro, in fixed3 rd, in fixed tmax, in fixed time, out fixed3 cma, out fixed3 uvw )
{
    cma = fixed3(0.0,0.0,0.0);
    uvw = fixed3(0.0,0.0,0.0);
    fixed matID = -1.0;

    fixed t = 1.0;
    
    // bounding volume test first
    fixed2 sph = iCylinderY( ro, rd, 1.5 );
  //vec2 sph = iConeY(ro-vec3(-0.05,3.7,0.35),rd,0.08);
    if( sph.y<0.0 ) return fixed2(-1.0,-1.0);
    
    // clip raymarch space to bonding volume
    tmax = min(tmax,sph.y);
    t    = max(1.0, sph.x);
    
    // raymarch
    for( int i=0; i<256; i++ )
    {
        fixed3 pos = ro + t*rd;

        fixed tmp;
        fixed4 h = map(pos,time,tmp,uvw);
        if( h.x<0.001 )
        {
            cma = h.yzw;
            matID = tmp;
            break;
        }
        t += h.x*0.95;
        if( t>tmax ) break;
    }

    return fixed2(t,matID);
}

// This is a replacement for a traditional dot(N,L) diffuse
// lobe (called N.L in the code) that fakes some subsurface
// scattering (transmision of light thorugh the skin that
// surfaces as a red glow)
//
fixed3 sdif( fixed ndl, fixed ir )
{
    fixed pndl = clamp( ndl, 0.0, 1.0 );
    fixed nndl = clamp(-ndl, 0.0, 1.0 );
    return fixed3(pndl,pndl,pndl) + fixed3(1.0,0.1,0.01)*0.7*pow(clamp(ir*0.75-nndl,0.0,1.0),2.0);
}

// Animates the eye central position (not the actual random
// darts). It's carefuly synched with the head motion, to
// make the eyes anticipate the head turn (without this
// anticipation, the eyes and the head are disconnected and
// it all looks like a zombie/animatronic)
//
fixed animEye( in fixed time )
{
    const fixed w = 6.1;
    fixed t = fmod(time-0.31,w*1.0);
    
    fixed q = frac((time-0.31)/(2.0*w));
    fixed s = (q > 0.5) ? 1.0 : 0.0;
    return (t<0.15)?1.0-s:s;
}

// Renders the girl. It finds the ray-girl intersection
// point, computes the normal at the intersection point,
// computes the ambient occlusion approximation, does per
// material setup (color, specularity, subsurface
// coefficient and paints some fake occlusion), and finally
// does the lighting computations.
//
// Lighting is not based on pathtracing. Instead the bounce
// light occlusion signals are created manually (placed
// and sized by hand). The subsurface scattering in the
// nose area is also painted by hand. There's not much
// attention to the physicall correctness of the light
// response and materials, but generally all signal do
// follow physically based rendering practices.
//
fixed3 renderGirl( in fixed2 p, in fixed3 ro, in fixed3 rd, in fixed tmax, in fixed3 col, in fixed time )
{
    // --------------------------
    // find ray-girl intersection
    // --------------------------
    fixed3 cma, uvw;
    fixed2 tm = intersect( ro, rd, tmax, time, cma, uvw );

    // --------------------------
    // shading/lighting	
    // --------------------------
    if( tm.y>0.0 )
    {
        fixed3 pos = ro + tm.x*rd;
        fixed3 nor = calcNormal(pos, time);

        fixed ks = 1.0;
        fixed se = 16.0;
        fixed tinterShadow = 0.0;
        fixed sss = 0.0;
        fixed focc = 1.0;
        //float frsha = 1.0;

        // --------------------------
        // material
        // --------------------------
        if( tm.y<1.5 ) // skin
        {
            fixed3 qos = fixed3(abs(uvw.x),uvw.yz);

            // base skin color
            col = lerp(fixed3(0.225,0.15,0.12),
                      fixed3(0.24,0.1,0.066),
                      smoothstep(0.4 ,0.0,length( qos.xy-fixed2(0.42,-0.3)))+
                      smoothstep(0.15,0.0,length((qos.xy-fixed2(0,-0.29))/fixed2(1.4,1))));
            
            // fix that ugly highlight
            col -= 0.03*smoothstep(0.13,0.0,length((qos.xy-fixed2(0,-0.49))/fixed2(2,1)));
                
            // lips
            col = lerp(col,fixed3(0.14,0.06,0.1),cma.x*step(-0.7,qos.y));
            
            // eyelashes
            col = lerp(col,fixed3(0.04,0.02,0.02)*0.6,0.9*cma.y);

            // fake skin drag
            uvw.y += 0.025*animData.x*smoothstep(0.3,0.1,length(uvw-fixed3(0.0,0.1,1.0)));
			uvw.y -= 0.005*animData.y*smoothstep(0.09,0.0,abs(length((uvw.xy-fixed2(0.0,-0.38))/fixed2(2.5,1.0))-0.12));
            
            // freckles
            fixed2 ti = floor(9.0+uvw.xy/0.04);
            fixed2 uv = frac(uvw.xy/0.04)-0.5;
            fixed te = frac(111.0*sin(1111.0*ti.x+1331.0*ti.y));
            te = smoothstep(0.9,1.0,te)*exp(-dot(uv,uv)*24.0); 
            col *= lerp(fixed3(1.1,1.1,1.1),fixed3(0.8,0.6,0.4), te);

            // texture for specular
            ks = 0.5 + 4.0*tex2D(_FourthTex,uvw.xy*1.1).x;
            se = 12.0;
            ks *= 0.5;
            tinterShadow = 1.0;
            sss = 1.0;
            ks *= 1.0 + cma.x;
            
            // black top
            col *= 1.0-smoothstep(0.48,0.51,uvw.y);
            
            // makeup
            fixed d2 = sdEllipsoid(qos-fixed3(0.25,-0.03,0.43),fixed3(0.37,0.42,0.4));
            col = lerp(col,fixed3(0.06,0.024,0.06),1.0 - smoothstep(0.0,0.03,d2));

            // eyebrows
    		{
            #if 0
            // youtube video version
        	fixed4 be = sdBezier( qos, fixed3(0.165+0.01*animData.x,0.105-0.02*animData.x,0.89),
                                     fixed3(0.37,0.18-0.005*animData.x,0.82+0.005*animData.x), 
                                     fixed3(0.53,0.15,0.69) );
            fixed ra = 0.005 + 0.015*sqrt(be.y);
            #else
            // fixed version
        	fixed4 be = sdBezier( qos, fixed3(0.16+0.01*animData.x,0.11-0.02*animData.x,0.89),
                                     fixed3(0.37,0.18-0.005*animData.x,0.82+0.005*animData.x), 
                                     fixed3(0.53,0.15,0.69) );
            fixed ra = 0.005 + 0.01*sqrt(1.0-be.y);
            #endif
            fixed dd = 1.0+0.05*(0.7*sin((sin(qos.x*3.0)/3.0-0.5*qos.y)*350.0)+
                                 0.3*sin((qos.x-0.8*qos.y)*250.0+1.0));
    		fixed d = be.x - ra*dd;
        	fixed mask = 1.0-smoothstep(-0.005,0.01,d);
        	col = lerp(col,fixed3(0.04,0.02,0.02),mask*dd );
        	}

            // fake occlusion
            focc = 0.2+0.8*pow(1.0-smoothstep(-0.4,1.0,uvw.y),2.0);
            focc *= 0.5+0.5*smoothstep(-1.5,-0.75,uvw.y);
            focc *= 1.0-smoothstep(0.4,0.75,abs(uvw.x));
            focc *= 1.0-0.4*smoothstep(0.2,0.5,uvw.y);
            
            focc *= 1.0-smoothstep(1.0,1.3,1.7*uvw.y-uvw.x);
            
            //frsha = 0.0;
        }
        else if( tm.y<2.5 ) // eye
        {
            // The eyes are fake in that they aren't 3D. Instead I simply
			// stamp a 2D mathematical drawing of an iris and pupil. That
			// includes the highlight and occlusion in the eyesballs.
            
            sss = 1.0;

            fixed3 qos = fixed3(abs(uvw.x),uvw.yz);
			fixed ss = sign(uvw.x);
            
            // iris animation
            fixed dt = floor(time*1.1);
            fixed ft = frac(time*1.1);
            fixed2 da0 = sin(1.7*(dt+0.0)) + sin(2.3*(dt+0.0)+fixed2(1.0,2.0));
            fixed2 da1 = sin(1.7*(dt+1.0)) + sin(2.3*(dt+1.0)+fixed2(1.0,2.0));
            fixed2 da = lerp(da0,da1,smoothstep(0.9,1.0,ft));

            fixed gg = animEye(time);
            da *= 1.0+0.5*gg;
            qos.yz = rot(qos.yz,da.y*0.004-0.01);
            qos.xz = rot(qos.xz,da.x*0.004*ss-gg*ss*(0.03-step(0.0,ss)*0.014)+0.02);

            fixed3 eos = qos-fixed3(0.31,-0.055 - 0.03*animData.x,0.45);
            
            // iris
            fixed r = length(eos.xy)+0.005;
            fixed a = atan2(ss*eos.x,eos.y);
            fixed3 iris = fixed3(0.09,0.0315,0.0135);
            iris += iris*3.0*(1.0-smoothstep(0.0,1.0, abs((a+3.14159)-2.5) ));
            iris *= 0.35+0.7*tex2D(_ThirdTex,fixed2(r,a/6.2831)).x;
            // base color
            col = fixed3(0.42,0.42,0.42);
            col *= 0.1+0.9*smoothstep(0.10,0.114,r);
            col = lerp( col, iris, 1.0-smoothstep(0.095,0.10,r) );
            col *= smoothstep(0.05,0.07,r);
			
            // fake occlusion backed in
            fixed edis = length((fixed2(abs(uvw.x),uvw.y)-fixed2(0.31,-0.07))/fixed2(1.3,1.0));
            col *= lerp( fixed3(1.0,1.0,1.0), fixed3(0.4,0.2,0.1), linearstep(0.07,0.16,edis) );

            // fake highlight
            qos = fixed3(abs(uvw.x),uvw.yz);
            col += (0.5-gg*0.3)*(1.0-smoothstep(0.0,0.02,length(qos.xy-fixed2(0.29-0.05*ss,0.0))));
            
            se = 128.0;

            // fake occlusion
            focc = 0.2+0.8*pow(1.0-smoothstep(-0.4,1.0,uvw.y),2.0);
            focc *= 1.0-linearstep(0.10,0.17,edis);
            //frsha = 0.0;
        }
        else if( tm.y<3.5 )// hoodie
        {
            sss = 0.0;
            col = fixed3(0.81*tex2D(_Volume1Tex,uvw*6.0).x);
            ks *= 2.0;
            
            // logo
            if( abs(uvw.x)<0.66 )
            {
                fixed par = length(uvw.yz-fixed2(-1.05,0.65));
                col *= lerp(fixed3(1.0,1.0,1.0),fixed3(0.6,0.2,0.8)*0.7,1.0-smoothstep(0.1,0.11,par));
                col *= smoothstep(0.005,0.010,abs(par-0.105));
            }                

            // fake occlusion
            focc = lerp(1.0,
                	   0.03+0.97*smoothstep(-0.15,1.7,uvw.z),
                       smoothstep(-1.6,-1.3,uvw.y)*(1.0-clamp(dot(nor.xz,normalize(uvw.xz)),0.0,1.0))
                      );
            
            //frsha = mix(1.0,
            //            clamp(dot(nor.xz,normalize(uvw.xz)),0.0,1.0),
            //            smoothstep(-1.6,-1.3,uvw.y)
            //           );
            //frsha *= smoothstep(0.85,1.0,length(uvw-vec3(0.0,-1.0,0.0)));
        }
        else if( tm.y<4.5 )// hair
        {
            sss = 0.0;
            col = (sin(cma.x)>0.7) ? fixed3(0.03,0.01,0.05)*1.5 :
                                     fixed3(0.04,0.02,0.01)*0.4;
            ks *= 0.75 + cma.z*18.0;
            fixed te = tex2Dlod( _ThirdTex,fixed4( 0.25*atan(uvw.x,uvw.y,fixed2(8.0*uvw.z) ,8.0*uvw.z) ))).x;
            col *= 2.0*te;
            ks *= 1.5*te;
            
			// fake occlusion
            focc  = 1.0-smoothstep(-0.40, 0.8, uvw.y);
            focc *= 1.0-0.95*smoothstep(-1.20,-0.2,-uvw.z);
            focc *= 0.5+cma.z*12.0;
            //frsha = 1.0-smoothstep(-1.3,-0.8,uvw.y);
            //frsha *= 1.0-smoothstep(-1.20,-0.2,-uvw.z);
        }
        else if( tm.y<5.5 )// teeth
        {
            sss = 1.0;
            col = fixed3(0.3,0.3,0.3);
            ks *= 1.5;
            //frsha = 0.0;
        }

        fixed fre = clamp(1.0+dot(nor,rd),0.0,1.0);
        fixed occ = focc*calcOcclusion( pos, nor, time );

        // --------------------------
        // lighting. just four lights
        // --------------------------
        fixed3 lin = fixed3(0.0,0.0,0.0);

        // fake sss
        fixed nma = 0.0;
        if( tm.y<1.5 )
        {
        nma = 1.0-smoothstep(0.0,0.12,length((uvw.xy-fixed2(0.0,-0.37))/fixed2(2.4,0.7)));
        }

        //vec3 lig = normalize(vec3(0.5,0.4,0.6));
        fixed3 lig = fixed3(0.57,0.46,0.68);
        fixed3 hal = normalize(lig-rd);
        fixed dif = clamp( dot(nor,lig), 0.0, 1.0 );
        //float sha = 0.0; if( dif>0.001 ) sha=calcSoftshadow( pos+nor*0.002, lig, 0.0001, 2.0, time, 5.0 );
        fixed sha = calcSoftshadow( pos+nor*0.002, lig, 0.0001, 2.0, time, 5.0 );
        fixed spe = 2.0*ks*pow(clamp(dot(nor,hal),0.0,1.0),se)*dif*sha*(0.04+0.96*pow(clamp(1.0-dot(hal,-rd),0.0,1.0),5.0));

        // fake sss for key light
        fixed3 cocc = lerp(fixed3(occ,occ,occ),
                        fixed3(0.1+0.9*occ,0.9*occ+0.1*occ*occ,0.8*occ+0.2*occ*occ),
                        tinterShadow);
        cocc = lerp( cocc, fixed3(1,0.3,0.0), nma);
        sha = lerp(sha,max(sha,0.3),nma);

        fixed3  amb = cocc*(0.55 + 0.45*nor.y);
        fixed bou = clamp(0.3-0.7*nor.x, 0.0, 1.0 );

        lin +=      fixed3(0.65,1.05,2.0)*amb*1.15;
        lin += 1.50*fixed3(1.60,1.40,1.2)*sdif(dot(nor,lig),0.5+0.3*nma+0.2*(1.0-occ)*tinterShadow) * lerp(fixed3(sha,sha,sha),fixed3(sha,0.2*sha+0.7*sha*sha,0.2*sha+0.7*sha*sha),tinterShadow);
        lin +=      fixed3(1.00,0.30,0.1)*sss*fre*0.6*(0.5+0.5*dif*sha*amb)*(0.1+0.9*focc);
        lin += 0.35*fixed3(4.00,2.00,1.0)*bou*occ*col;

        col = lin*col + spe + fre*fre*fre*0.1*occ;

        // overall
		col *= 1.1;
    }
        
    return col;
}

// Animates the head turn. This is my first time animating
// and I am aware I'm in uncanny/animatronic land. But I
// have to start somwhere!
//
fixed animTurn( in fixed time )
{	
    const fixed w = 6.1;
    fixed t = fmod(time,w*2.0);
    
    fixed3 p = (t<w) ? fixed3(0.0,0.0,1.0) : fixed3(w,1.0,-1.0);
    return p.y + p.z*expSustainedImpulse(t-p.x,1.0,10.0);
}

// Animates the eye blinks. Blinks are motivated by head
// turns (again, in an attempt tp avoid animatronic and
// zoombie feel), but also there are random blinks. This
// same funcion is called with some delay and extra
// smmoothness to get the blink of the eyes be followed by
// the face muscles around the face.
//
fixed animBlink( in fixed time, in fixed smo )
{
    // head-turn motivated blink
    const fixed w = 6.1;
    fixed t = fmod(time-0.31,w*1.0);
    fixed blink = smoothstep(0.0,0.1,t) - smoothstep(0.18,0.4,t);

    // regular blink
    fixed tt = fmod(1.0+time,3.0);
    blink = max(blink,smoothstep(0.0,0.07+0.07*smo,tt)-smoothstep(0.1+0.04*smo,0.35+0.3*smo,tt));
    
    // keep that eye alive always
    fixed blinkBase = 0.04*(0.5+0.5*sin(time));
    blink = lerp( blinkBase, 1.0, blink );

    // base pose is a bit down
    fixed down = 0.15;
    return down+(1.0-down)*blink;
}

// The main rendering entry point. Basically it does some
// setup, creating the ray that will explore the 3D scene
// in search of the girl for each pixel, computes the
// animation variables (blink, mouth and head movements),
// does the rendering of the girl if it finds her under
// the current pixel, and finally does gamme correction
// and some minimal color processing and vignetting to the
// image.
//




	VertexOutput vert (VertexInput v)
	{
		VertexOutput o;
		o.pos = UnityObjectToClipPos (v.vertex);
		o.uv = v.uv;
		//VertexFactory
		return o;
	}

	fixed4 frag(VertexOutput i) : SV_Target
	{
		
    // render
    fixed3 tot = fixed3(0.0,0.0,0.0);
    
    #if AA>1
    for( int m=ZERO; m<AA; m++ )
    for( int n=ZERO; n<AA; n++ )
    {
        // pixel coordinates
        fixed2 o = fixed2(fixed(m),fixed(n)) / fixed(AA) - 0.5;
        fixed2 p = (-1 + 2.0*(i.uv+o))/1;
        fixed d = 0.5*sin(i.uv.x*147.0)*sin(i.uv.y*131.0);
        fixed time = _Time.y - 0.5*(1.0/24.0)*(fixed(m*AA+n)+d)/fixed(AA*AA-1);
        #else    
        fixed2 p = (-1 + 2.0*i.uv)/1;
        fixed time = _Time.y;
        #endif
        
        time += 2.0;
        
        // camera movement	
        fixed3 ro; fixed fl;
        fixed3x3 ca = calcCamera( time, ro, fl );
    	fixed3 rd = ca * normalize( fixed3((p-fixed2(-0.52,0.12))/1.1,fl));

        // animation (blink, face follow up, mouth)
        fixed turn = animTurn( time );
        animData.x = animBlink(time,0.0);
        animData.y = animBlink(time-0.02,1.0);
        animData.z = -0.25 + 0.2*(1.0-turn)*smoothstep(-0.3,0.9,sin(time*1.1)) + 0.05*cos(time*2.7);

        // animation (head orientation)
        animHead = fixed3( sin(time*0.5), sin(time*0.3), -cos(time*0.2) );
        animHead = animHead*animHead*animHead;
        animHead.x = -0.025*animHead.x + 0.2*(0.7+0.3*turn);
        animHead.y =  0.1 + 0.02*animHead.y*animHead.y*animHead.y;
        animHead.z = -0.03*(0.5 + 0.5*animHead.z) - (1.0-turn)*0.05;
        
        // rendering
        fixed4 tmp = tex2D(_SecondTex,fixed2(i.uv,i.uv),0);
        fixed3 col = tmp.xyz;
        fixed tmin = tmp.w;
        
        if( p.x*1.4+p.y<0.8 && -p.x*4.5+p.y<6.5 && p.x<0.48)
        col = renderGirl(p,ro,rd,tmin,col,time);
        //else col=vec3(0,1,0);
        
        // gamma        
        col = pow( col, fixed3(0.4545,0.4545,0.4545) );
	    tot += col;
    #if AA>1
    }
    tot /= fixed(AA*AA);
    #endif
 
    // compress
    tot = 3.8*tot/(3.0+dot(tot,fixed3(0.333,0.333,0.333)));
  
    // vignette
    fixed2 q = i.uv/1;
    tot *= 0.5 + 0.5*pow(16.0*q.x*q.y*(1.0-q.x)*(1.0-q.y),0.15);

    // grade
    tot = tot*fixed3(1.02,1.00,0.99)+fixed3(0.0,0.0,0.045);
       
    return fixed4( tot, 1.0 );

	}
	ENDCG
	}
  }
}
