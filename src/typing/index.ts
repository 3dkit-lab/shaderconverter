type renderpassType =
  | "image"
  | "buffer"
  | "common"
  | "keyboard"
  | "mouse"
  | "sound"
  | "cubemap";
export interface renderpass {
  code: string;
  description: string;
  inputs: input[];
  name: string;
  outputs: output[];
  type: renderpassType;
}

export interface input {
  channel: number;
  filepath: string;
  id: string;
  previewfilepath: string;
  published: number;
  sampler: sampler;
  type: renderpassType;
}

export interface output {
  id: string;
  channel: number;
}

interface sampler {
  filter: string;
  wrap: string;
  vflip: string;
  srgb: string;
  internal: string;
}
