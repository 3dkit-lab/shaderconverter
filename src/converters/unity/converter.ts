import Handlebars from "handlebars";
import { UnityBaseShader } from "./baseShader";
import { shader } from "@/typing";
import { useCommentProtector } from "@/utils";

enum types {
    Texture,
    Int,
    Float,
    Vector,
    Color,
}

export const toUnityShader = (name: string, shader: shader) => {
    const { protectComments, restoreComments } = useCommentProtector();
    const { data } = shader;
    const { image, common } = data;
    let { code: glsl } = image;
    if (common) {
        const { code: commonGlsl } = common;
        glsl = commonGlsl + glsl;
    }
    glsl = protectComments(glsl);
    const template = Handlebars.compile(UnityBaseShader, { noEscape: true });
    const { properties, variables, functions, mainImage } = parseShader(glsl);
    glsl = template({ name, properties, functions, mainImage, variables });
    return restoreComments(glsl);
};

const parseShader = (shader: string) => {
    const text = updateTemplate(shader);
    const mainImage = extractMainImage(text);
    const functionsText = text.match(/(.*)(?=void mainImage)/ms);
    const functions = functionsText ? functionsText[1] : "";
    const { properties, variables } = parseProperties(text);
    return { mainImage, functions, properties, variables };
};

const parseProperties = (shader: string) => {
    const propertyConfig = [
        { key: "_MainTex", type: types.Texture },
        { key: "_SecondTex", type: types.Texture },
        { key: "_ThirdTex", type: types.Texture },
        { key: "_FourthTex", type: types.Texture },
        { key: "iDate", type: types.Vector },
        { key: "iMouse", type: types.Vector },
    ];
    let properties = "",
        variables = "";
    for (const item of propertyConfig) {
        const { key, type } = item;
        if (shader.includes(key)) {
            const property = decelarateProperty(key, type);
            const { name, variableType, initialize, correspondingVariable } =
                property;
            properties += `${name} ("${name.replace("_", "")}", ${variableType}) = ${initialize}\n`;
            variables += `${correspondingVariable}\n`;
        }
    }
    return { properties, variables };
};

const decelarateProperty = (name: string, type: types) => {
    let variableType = "";
    let initialize = "";
    let correspondingVariable = "";
    switch (type) {
        case types.Int:
            variableType = "int";
            correspondingVariable = "int";
            initialize = "0";
            break;
        case types.Float:
            variableType = "float";
            correspondingVariable = "float";
            initialize = "0";
            break;
        case types.Texture:
            variableType = "2D";
            correspondingVariable = "sampler2D";
            initialize = `"white" {}`;

            break;
        case types.Color:
            variableType = "Color";
            correspondingVariable = "float4";
            initialize = "(0,0,0,0)";
            break;
        case types.Vector:
            variableType = "Vector";
            correspondingVariable = "float4";
            initialize = "(0,0,0,0)";
            break;
        default:
            variableType = "int";
            correspondingVariable = "int";
            initialize = "0";
            break;
    }
    correspondingVariable += ` ${name};`; //for example sampler2D _MainTex;
    return { name, variableType, initialize, correspondingVariable };
};

const extractMainImage = (code: string) => {
    const start = code.indexOf("void mainImage(");
    if (start === -1) return null;

    const braceStart = code.indexOf("{", start);
    if (braceStart === -1) return null;

    let depth = 1;
    let content = "";
    for (let i = braceStart + 1; i < code.length; i++) {
        if (code[i] === "{") depth++;
        else if (code[i] === "}") depth--;

        if (depth === 0) {
            content = code.slice(braceStart + 1, i);
            break;
        }
    }
    return content;
};

const updateTemplate = (text: string) => {
    Handlebars.registerHelper("regexReplace", function (text, regexMap) {
        let result = text;
        for (const [regex, replacement] of Object.entries(regexMap)) {
            const replaceRegex = new RegExp(regex, "g");
            result = result.replace(replaceRegex, replacement);
        }

        return result;
    });
    const data = {
        text,
        regexMap: {
            "vec|half|float": "fixed",
            "mix": "lerp",
            "iGlobalTime": "_Time.y",
            "fragColor =": "return",
            "fract": "frac",
            "ifixed(\\d)": "fixed$1", //ifixed to fixed
            "texture": "tex2D",
            "tex2DLod": "tex2Dlod",
            "refrac": "refract",
            "iChannel0": "_MainTex",
            "iChannel1": "_SecondTex",
            "iChannel2": "_ThirdTex",
            "iChannel3": "_FourthTex",
            "iResolution.((x|y){1,2})?": "1",
            "fragCoord.xy / iResolution.xy": "i.uv",
            "fragCoord(.xy)?": "i.uv",
            "iResolution(\\.(x|y){1,2})?": "1",
            "iMouse": "_iMouse",
            "mat2": "fixed2x2",
            "mat3": "fixed3x3",
            "mat4": "fixed4x4",
            "mod": "fmod",
            // "for\\(": "[unroll(100)]\nfor(",
            "iTime": "_Time.y",
            "(tex2Dlod\\()([^,]+\\,)([^)]+\\)?[)]+.+(?=\\)))":
                "$1$2float4($3,0)",
            "fixed4\\(([^(,]+?)\\)": "fixed4($1,$1,$1,$1)",
            "fixed3\\(([^(,]+?)\\)": "fixed3($1,$1,$1)",
            "fixed2\\(([^(,]+?)\\)": "fixed2($1,$1)",
            "tex2D\\(([^,]+)\\,\\s*fixed2\\(([^,].+)\\)\\,(.+)\\)":
                "tex2Dlod($1,fixed4($2,fixed2($3,$3)))", //when vec3 col = texture( iChannel0, vec2(uv.x,1.0-uv.y), lod ).xyz; -> https://www.shadertoy.com/view/4slGWn
            "texelFetch": "tex2D", //badan bokonesh texlod
            "atan\\(([^,]+?)\\,([^,]+?)\\)": "atan2($2,$1)", //badan bokonesh texlod
            "gl_FragCoord":
                "((i.screenCoord.xy/i.screenCoord.w)*_ScreenParams.xy)",
        },
    };

    const template = Handlebars.compile(`
        {{{regexReplace text regexMap}}}
      `);
    return template(data);
};
