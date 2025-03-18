import Handlebars from "handlebars";
import { generateGuid } from "@/utils";
import { BaseMeta, NativeFormatImporter, ShaderImporter } from "./baseMeta";
import { UnityBaseMaterial } from "./baseMaterial";
import { toUnityShader } from "./converter";
import { shaderOptions } from "@/typing";

export const generateFile = (shadertoy: shaderOptions) => {
    const { name, renderpass, multipass } = shadertoy;
    if (multipass) {
        // todo
    } else {
        const [mainImage] = renderpass;
        const { code: shader } = mainImage;
        const shaderFileGuid = generateGuid();
        const shaderMetaFile = generateShaderMeta(shaderFileGuid);
        const materialFileGuid = generateGuid();
        const materialMetaFile = generateMaterialMeta(materialFileGuid);
        const shaderFile = toUnityShader(name, shader);
        const materialFile = generateMaterial(shaderFileGuid);
        return [shaderMetaFile, shaderFile, materialMetaFile, materialFile];
    }
};

const generateShaderMeta = (guid: string) => {
    const template = Handlebars.compile(BaseMeta);
    return template({
        Importer: ShaderImporter,
        guid,
        createTime: Math.floor(Date.now() / 1000),
    });
};

const generateMaterialMeta = (guid: string) => {
    const template = Handlebars.compile(BaseMeta);
    return template({
        guid,
        createTime: Math.floor(Date.now() / 1000),
        Importer: NativeFormatImporter,
    });
};

const generateMaterial = (guid: string) => {
    const template = Handlebars.compile(UnityBaseMaterial);
    return template({ guid });
};
