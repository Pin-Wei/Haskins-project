#!/usr/bin/env python
# -*- coding: utf-8 -*- 

# python Compute_char_SC.py --model_type 1

import os
import pandas as pd
import numpy as np
from scipy.spatial.distance import pdist, squareform
from sklearn.metrics.pairwise import cosine_similarity
import argparse

parser = argparse.ArgumentParser(description="")
parser.add_argument("--model_type", type=int, 
                    choices=[0, 1, 2], default=1, 
                    help='''
    [0] "CKIP_w2v",     # Pre-trained word2vec (skip-gram) model based on Chinese Gigaword and Sinica Corpus
    [1] "CKIP_GloVe",   # Global Vectors for Word Representation
    [2] "TencentAI_DSG" # Directional Skip-Gram (developed by Tencent AI Lab)
    [3] "CKIP_BERT",    # Bidirectional Encoder Representations from Transformers  
    [4] "CKIP_ALBERT",  # A Lite BERT 
    [5] "CKIP_GPT2",    # Generative Pre-trained Transformer
''')
args = parser.parse_args()
model_type = [
    "CKIP_w2v",     # Pre-trained word2vec (skip-gram) model based on Chinese Gigaword and Sinica Corpus
    "CKIP_GloVe",   # Global Vectors for Word Representation
    "TencentAI_DSG" # Directional Skip-Gram (developed by Tencent AI Lab)
    # "CKIP_BERT",    # Bidirectional Encoder Representations from Transformers  
    # "CKIP_ALBERT",  # A Lite BERT 
    # "CKIP_GPT2",    # Generative Pre-trained Transformer
][args.model_type]

if (model_type == "CKIP_w2v") or (model_type == "CKIP_GloVe"): 
    import torch
    import torchtext.vocab

elif model_type == "TencentAI_DSG":
    from gensim.models import KeyedVectors
    ## Gensim: https://hackmd.io/@gensimDART/SydKygQa_#Gensim-%E4%BB%8B%E7%B4%B9-Introduction-to-Gensim
    
elif model_type == "CKIP_GPT2":
    from transformers import BertTokenizerFast, AutoModelForCausalLM
    ## HuggingFace's transformers library, if not installed: !pip install -U ckip-transformers

else:
    from transformers import BertTokenizerFast, AutoModelForMaskedLM
    ## see more: https://github.com/ckiplab/ckip-transformers

# Setup configurations: ----------------------------------------------------------
class Config:
    char_folder      = "Data_single_characters"
    char_DF_path     = os.path.join(char_folder, "Chang_Lee_2016.csv")
    char_Hsieh_path  = os.path.join(char_folder, "Hsieh_family_semantic_consistency.csv")
    word_folder      = "Data_compound_words"
    word_Sinica_path = os.path.join(word_folder, "words_sinica_v4_edited.csv") # Sinica Corpus 4.0
    word_Tse_path    = os.path.join(word_folder, "CLP_data.xlsx") # Tse et al. (2017, 2023, 2024)
    out_folder       = "SC_matrices"
    char_list_path   = os.path.join(out_folder, ["new_char_list.txt", "char_list.txt"][0])
    main_out_path    = os.path.join(out_folder, f"char_consistency_metrics_{model_type.split('_')[-1]}.xlsx") 
    comp_out_path    = os.path.join(out_folder, f"compare_con_metrics_{model_type.split('_')[-1]}.xlsx")

config = Config()

if (model_type == "CKIP_w2v") or (model_type == "CKIP_GloVe"):
    config.vocab_path = "/home/aclexp/.local/lib/python3.10/site-packages/torchtext/vocab"
    
    if model_type == "CKIP_w2v":
        config.ckip_word_embeddings = "w2v_CNA_ASBC_300d.vec"

    elif model_type == "CKIP_GloVe":
        config.ckip_word_embeddings = "Glove_CNA_ASBC_300d.vec"
            ## see: https://ckip.iis.sinica.edu.tw/project/embedding
            ## download from: https://ckipsvr.iis.sinica.edu.tw/cemb/reg.php

elif model_type == "TencentAI_DSG":
    config.tencentAI_embeddings = ["1000000-small_trad.txt", "1000000-small_trad.bin"]
        ## download from: https://github.com/cliuxinxin/TX-WORD2VEC-SMALL

## Load list of characters: ------------------------------------------------------
if os.path.isfile(config.char_list_path):
    with open(config.char_list_path, 'r') as f:
        char_list = [char.strip("\n") for char in f.readlines()]
else:
    char_DF = pd.read_csv(config.char_DF_path)
    char_list = char_DF.Character.to_list()
    with open(config.char_list_path, 'w') as f:
        f.writelines( [f"{char}\n" for char in char_list] )

# ## Load list of words (with frequencies): ----------------------------------------
# match ["Sinica", "Tse"][1]:
#     case "Sinica":
#         word_DF = pd.read_csv(config.word_Sinica_path)
#         # word_DF = word_DF.iloc[:, :6]
#         word_list = word_DF["word"].to_list()
#     case "Tse":
#         word_DF = pd.read_excel(config.word_Tse_path)
#         # word_DF = word_DF.loc[:, ["Word_Trad", "Word_Sim", "Freq_C1", "Freq_C2", "Freq_word"]]
#         word_list = word_DF["Word_Trad"].to_list()

## Load word embeddings (from Tencent AI Lab): -----------------------------------
if (model_type == "CKIP_w2v") or (model_type == "CKIP_GloVe"): 
    model = torchtext.vocab.Vectors(config.ckip_word_embeddings, cache=config.vocab_path)
    word_embeddings = {}
    for word, word_index in model.stoi.items():
        word_embeddings[word] = model.vectors[word_index]
    
elif model_type ==  "CKIP_BERT": 
    tokenizer = BertTokenizerFast.from_pretrained('bert-base-chinese')
    model = AutoModelForMaskedLM.from_pretrained(
        ['ckiplab/bert-tiny-chinese', 'ckiplab/bert-base-chinese'][0])
    embeddings = model.get_input_embeddings()
    word_embeddings = {}
    for word in word_list:
        word_embeddings[word] = embeddings(torch.tensor(tokenizer.encode(word)))

elif model_type == "CKIP_ALBERT":
    tokenizer = BertTokenizerFast.from_pretrained('bert-base-chinese')
    model = AutoModelForMaskedLM.from_pretrained(
        ['ckiplab/albert-tiny-chinese', 'ckiplab/albert-base-chinese'][0])
    embeddings = model.get_input_embeddings()

elif model_type == "CKIP_GPT2": 
    tokenizer = BertTokenizerFast.from_pretrained('bert-base-chinese')
    model = AutoModelForCausalLM.from_pretrained(
        ['ckiplab/gpt2-tiny-chinese', 'ckiplab/gpt2-base-chinese'][0])
    embeddings = model.get_input_embeddings()

elif model_type == "TencentAI_DSG": 
    # word_embeddings = KeyedVectors.load_word2vec_format(config.tencentAI_embeddings[0], binary=False)
    # word_embeddings.save(config.tencentAI_embeddings[1])
    # model = KeyedVectors.load_word2vec_format(config.tencentAI_embeddings[1])
    word_embeddings = {}
    with open(config.tencentAI_embeddings[0], encoding='utf8') as f:
        for index, line in enumerate(f):
            if index == 0:
                continue
            values = line.split(' ')
            word = values[0]
            coefs = np.asarray(values[1:], dtype='float32')
            word_embeddings[word] = coefs

# Main: --------------------------------------------------------------------------
output_dict = {}

def calculate_matrix_triu(matrix):
    upper_triangle_indices = np.triu_indices_from(matrix, k=1)
    upper_triangle_values = matrix[upper_triangle_indices]
    return np.mean(upper_triangle_values), np.var(upper_triangle_values)

# ## Filter loaded word embeddings by word_list:
# word_embeddings = {
#     word: embedding for word, embedding in word_embeddings.items() if word in word_list }

for char in char_list: 
    ## Select embeddings of words that contain the specified character:
    selected_embeddings = [ embedding for word, embedding in word_embeddings.items() if char in word ]

    if len(selected_embeddings) < 2: 
        print(f"less than two word contain '{char}', similarity matrix cannot be calculated.")
        # output_dict[char] = 0
    else: 
        distance_matrix = squareform(pdist(selected_embeddings, metric="cosine"))
        dis_mean, dis_var = calculate_matrix_triu(distance_matrix)
        similarity_matrix = cosine_similarity(selected_embeddings)
        sim_mean, sim_var = calculate_matrix_triu(similarity_matrix)
        output_dict[char] = [
            len(selected_embeddings), sim_mean, sim_var, dis_mean, dis_var
        ]

output = pd.DataFrame.from_dict(
    output_dict, orient='index', 
    columns=["Family.N", "Se.Sim.Mean", "Se.Sim.Var", "Se.Dis.Mean", "Se.Dis.Var"])
output.insert(0, "Char", output.index)
output.to_excel(config.main_out_path, index=False)

# Hsieh_DF = pd.read_csv(config.char_Hsieh_path)
# (Hsieh_DF
#  .loc[:, ["character", "cons_type_PG", "cons_token_PG"]]
#  .rename(columns={"character": "Char"})
#  .merge(output, on="Char")
#  .to_excel(config.comp_out_path, index=False))

# cd /media/data2/pinwei/Haskins_project/Derivatives
# for x, name in enumerate(["w2v", "GloVe", "DSG"]):
# df = pd.read_excel(f"compare_con_metrics_{name}.xlsx")
# df = df.rename(columns={"Se.Sim.Mean": f"SC.{name}", "Family.N": f"NbS.{name}"})
# if x == 0: 
# df = df.rename(columns={"cons_type_PG": "SC.Hsieh"})
# df_merged = df.loc[:, ["Char", "SC.Hsieh", f"NbS.{name}", f"SC.{name}"]]
# else:
# df_merged = df_merged.merge(df.loc[:, ["Char", f"NbS.{name}", f"SC.{name}"]], on="Char")
# df_merged.to_excel("compare_con_metrics_merged.xlsx", index=False)
# df_merged.iloc[:, [1,3,5,7]].corr().to_excel("compare_con_metrics_merged_pwc.xlsx")