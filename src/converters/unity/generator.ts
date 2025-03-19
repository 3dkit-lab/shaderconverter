import Handlebars from "handlebars";
import { generateGuid } from "@/utils";
import { BaseMeta, NativeFormatImporter, ShaderImporter } from "./baseMeta";
import { UnityBaseMaterial } from "./baseMaterial";
import { toUnityShader } from "./converter";
import { shader } from "@/typing";

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

export const generate = (name: string, shader: shader) => {
    // const { name, multipass } = shader;
    // let shaderFile = "";
    // if (multipass) {
    //     // todo
    // } else {
    // }
    const shaderFile = toUnityShader(name, shader);
    const shaderFileGuid = generateGuid();
    const shaderMetaFile = generateShaderMeta(shaderFileGuid);
    const materialFileGuid = generateGuid();
    const materialMetaFile = generateMaterialMeta(materialFileGuid);
    const materialFile = generateMaterial(shaderFileGuid);
    return [shaderMetaFile, shaderFile, materialMetaFile, materialFile];
};
