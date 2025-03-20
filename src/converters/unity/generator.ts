import Handlebars from "handlebars";
import { generateGuid } from "@/utils";
import {
    BaseMeta,
    NativeFormatImporter,
    ShaderImporter,
    TextureImporter,
} from "./baseMeta";
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

const generateImageMeta = (guid: string) => {
    const template = Handlebars.compile(BaseMeta);
    return template({
        Importer: TextureImporter,
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

    // m_TexEnvs: []
    return template({ guid });
};

// const generateImage = (fileName: string) => {
// const template = Handlebars.compile(BaseMeta);
// return template({
//     Importer: TextureImporter,
//     guid: generateGuid(),
//     fileName,
// });
// };

export const generate = (name: string, shader: shader) => {
    // const { name, multipass } = shader;
    // let shaderFile = "";
    // if (multipass) {
    //     // todo
    // } else {
    // }
    const shaderFile = toUnityShader(name, shader);
    const shaderFileGuid = generateGuid();
    const donwloadList = [];
    const internalList = [];
    const { fileList } = shader.data;
    for (const input of fileList) {
        const { type } = input;
        if (type == "texture") {
            const guid = generateGuid();
            input[`guid`] = guid;
            donwloadList.push(input);
            internalList.push(generateImageMeta(guid));
        }
    }

    // const imageFileGuid = generateGuid();
    const shaderMetaFile = generateShaderMeta(shaderFileGuid);
    const materialFileGuid = generateGuid();
    const materialMetaFile = generateMaterialMeta(materialFileGuid);
    const materialFile = generateMaterial(shaderFileGuid);
    return [shaderMetaFile, shaderFile, materialMetaFile, materialFile];
};
