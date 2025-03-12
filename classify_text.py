import torch
from transformers import pipeline

def classify_text(texts, model_choice):
  if model_choice == "xlm-roberta":
  model_name = "xlm-roberta-large-finetuned-conll03-english"
elif model_choice == "mbert":
  model_name = "bert-base-multilingual-cased"
elif model_choice == "xlnet":
  model_name = "xlnet-base-cased"
else:
  raise ValueError("Invalid model choice")

classifier = pipeline("text-classification", model=model_name, tokenizer=model_name)

predictions = classifier(texts, truncation=True)
return [pred["score"] for pred in predictions]
