Shader "ShaderConverter/Bubbles"
	{

	Properties{
	
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

	

	
        // Copyright Inigo Quilez, 2013 - https://iquilezles.org/
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
		
	fixed2 uv = (2.0*i.uv-1) / 1;

    // background	 
	fixed3 color = fixed3(0.8 + 0.2*uv.y,0.8 + 0.2*uv.y,0.8 + 0.2*uv.y);

    // bubbles	
	for( int i=0; i<40; i++ )
	{
        // bubble seeds
		fixed pha =      sin(fixed(i)*546.13+1.0)*0.5 + 0.5;
		fixed siz = pow( sin(fixed(i)*651.74+5.0)*0.5 + 0.5, 4.0 );
		fixed pox =      sin(fixed(i)*321.55+4.1) * 1 / 1;

        // bubble size, position and color
		fixed rad = 0.1 + 0.5*siz;
		fixed2  pos = fixed2( pox, -1.0-rad + (2.0+2.0*rad)*fmod(pha+0.1*_Time.y*(0.2+0.8*siz),1.0));
		fixed dis = length( uv - pos );
		fixed3  col = lerp( fixed3(0.94,0.3,0.0), fixed3(0.1,0.4,0.8), 0.5+0.5*sin(fixed(i)*1.2+1.9));
		//    col+= 8.0*smoothstep( rad*0.95, rad, dis );
		
        // render
		fixed f = length(uv-pos)/rad;
		f = sqrt(clamp(1.0-f*f,0.0,1.0));
		color -= col.zyx *(1.0-smoothstep( rad*0.95, rad, dis )) * f;
	}

    // vigneting	
	color *= sqrt(1.5-0.5*length(uv));

	return fixed4(color,1.0);

	}
	ENDCG
	}
  }
}
