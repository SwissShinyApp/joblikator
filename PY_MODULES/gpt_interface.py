# control pricing: https://platform.openai.com/account/billing/overview
# import the libraries
import json
import openai
import sys
import subprocess
import os
import re
import time


# returns the language in English
def get_lang(language = "en"):
  if language == "en":
      return "English"
  elif language == "de":
      return "German"
  elif language == "pt":
      return "Portuguese"
  else:
      return "English"

def test(temp):
  answer_title = generate_answer(message, temp)
  title = parse_anwer(answer_title)
  print(title[0])
  del answer_title
  del title

  answer_title = generate_answer(message, temp)
  title = parse_anwer(answer_title)
  print(title[0])
  del answer_title
  del title

  answer_title = generate_answer(message, temp)
  title = parse_anwer(answer_title)
  print(title[0])
  del answer_title
  del title

  answer_title = generate_answer(message, temp)
  title = parse_anwer(answer_title)
  print(title[0])
  del answer_title
  del title
  answer_title = generate_answer(message, temp)
  title = parse_anwer(answer_title)
  print(title[0])
  del answer_title
  del title
  answer_title = generate_answer(message, temp)
  title = parse_anwer(answer_title)
  print(title[0])
  del answer_title
  del title
  answer_title = generate_answer(message,temp)
  title = parse_anwer(answer_title)
  print(title[0])
  del answer_title
  del title
  answer_title = generate_answer(message, temp)
  title = parse_anwer(answer_title)
  print(title[0])
  del answer_title
  del title
  answer_title = generate_answer(message, temp)
  title = parse_anwer(answer_title)
  print(title[0])
  del answer_title
  del title
  answer_title = generate_answer(message,temp)
  title = parse_anwer(answer_title)
  print(title[0])
  del answer_title
  del title
  
  
  # calling GPT and parsing
  lines = parse_anwer(answer)
  # adjust the lenght
  
  lines = adjust_len(lines)

# just to test
def print_res(lines_orig, lines_short):
  print(len(lines_short[0]))
  print(lines_short[0])
  print(len(lines_orig[0]))
  print(lines_orig[0])
  print(len(lines_short[1]))
  print(lines_short[1])
  print(len(lines_orig[1]))
  print(lines_orig[1])
  print(len(lines_short[2]))
  print(lines_short[2])
  print(len(lines_orig[2]))
  print(lines_orig[2])
  
# just to test
def print_res2(lines):
  print(len(lines[0]))
  print(lines[0])
  print(len(lines[1]))
  print(lines[1])
  print(len(lines[2]))
  print(lines[2])
  
  
## -- ===== DATA IMPORT ====== ---

# imports params from R
def import_params():
  # import the complete messages string
  path = 'PY_MODULES/params.json'
  with open(path, "r") as file:
    params = json.load(file)
  
  parsed_params = {}
  for key, value in params.items():
    parsed_params[key] = value[0]
  return parsed_params

# imports the job description
def import_job(job_path = 'PY_MODULES/job_desc.txt'):
  #job_path = 'PY_MODULES/job_desc.txt'
  with open(job_path, 'r') as file:
    # Read the contents of the file
    job_desc = file.read()
  return job_desc

# imports the CV
def import_cv(cv_path = 'PY_MODULES/cv.txt'):
 # language = language.lower()
  
  #cv_path = f'{project_dir}/data/cv_data/{cv_type}/cv_txt_{cv_type}_{language}.txt'
  #cv_path = 'PY_MODULES/cv.txt'
  with open(cv_path, 'r') as file:
    # Read the contents of the file
    cv = file.read()
  return cv

# imports the CV
def import_txt(file = 'cv'):
  path = f'PY_MODULES/{file}.txt'
  with open(path, 'r') as file:
    # Read the contents of the file
    cv = file.read()
  return cv


## -- ===== DATA EXTRACT & CLEAN ====== ---

# extract answer data
def parse_anwer(answer):
  text = answer[0]
  lines = text.split('\n')
  pattern = r'^[\W\d\s_]+(?=[a-zA-Z])'
  lines_clean = [re.sub(pattern, '', line) for line in lines]
  return lines_clean

# checks if GPT response is negative
def check_negative_return(lst):
    keywords = ["no", "not", "none"]
    cleaned_text = re.sub(r'[^a-zA-Z0-9\s]', ' ', lst[0])
    cleaned_text_lower = cleaned_text.lower()
    for keyword in keywords:
        pattern = r'\b{}\b'.format(re.escape(keyword.lower()))
        if re.search(pattern, cleaned_text_lower):
            return [False]
    return lst

def remove_parentheses(txt):
    pattern = r'\([^)]*\)'
    result = re.sub(pattern, '', txt)
    return result.strip()


## -- ===== CORRECT THE LENGTH ====== ---

def make_longer(txt, temp):
  prompt = f'Make the following text longer to be more than 40 and less than 65 characters long, without changing the meaning: {txt}. The text length must be exactly between 40 and 65 characters.'
  ans = generate_answer([{"role": "system", "content": prompt}], temp)
  line_ret = parse_anwer(ans)
  #return line_ret[0]

  return remove_parentheses(line_ret[0])

# makes text shorter if too long
def make_shorter(txt, temp, min = 45, max = 65):
  prompt = f"Make the following text shorter to be less than {65} and more than {45} characters long, without changing the meaning: '{txt}'. The text length must be exactly between {40} and {65} characters."
  ans = generate_answer([{"role": "system", "content": prompt}], temp)
  line_ret = parse_anwer(ans)
  #return line_ret[0]
  return remove_parentheses(line_ret[0])

# extends or shortens the lines
def adjust_len(lines):
  # return lines
  max_iterations = 3
  for j in range(max_iterations):
    all_within_range = True
    for i in range(len(lines)):
      if len(lines[i]) > 65:
        lines[i] = make_shorter(lines[i], 0.5)
        all_within_range = False
      elif len(lines[i]) < 36:
        lines[i] = make_longer(lines[i], 0.5)
        all_within_range = False
    if all_within_range:
      break

  return lines

## -- ===== PROMPT ENGINEERING ====== ---

# create the main prompt for Highlights and stores it
def hl_prompt():
  # Read the job
  job_desc = import_job()
  # Read the cv
  cv = import_cv()
  
  # concat the prompt
  prompt = f"""
  Job Description: {job_desc}.

  Candidate Resume: {cv}.

  Based on the provided Job Description, identify the 3 most important skills from the Candidate Resume that maximize the candidate's chances of success. Return a list with 3 succinct attention-grabbing bullet points for the CV's header.
  """
  # Based on the provided Job Description, identify the top three skills from the Candidate Resume that best align with the specific requirements of the Job Description. Emphasize these skills to maximize the candidate's chances of success. All mentioned skills must be in the Candidate Resume. Present the three skills in the form of attention-grabbing bullet points.
  #prompt = f"Considering the following job description: {job_desc} and the following resume: {cv}, which 3 skills of the resume should be highlighted to maximize the chances? Focus thereby strongly on the specific requirements from the job description and the best matching skills. Return the 3 skills in the form of catchy bullet points."
  return prompt
  #return("What is Canada?")

# takes str prompt and creates message and stores json
def initial_message(prompt):
  # prep make the message + send request to gpt
  message=[
            {"role": "system", "content": "You are a recruiter."},
            {"role": "user", "content": prompt}
        ]

  # Convert the list to JSON string
  message_json = json.dumps(message)

  # Write the JSON string to a file
  path = 'PY_MODULES/prompt_message_string.json'
  with open(path, "w") as file:
    file.write(message_json)
  return path

# appends a new prompt to the message string and stores it again
def append_message(prompt_add):
  # Read the JSON data from the file
  path = 'PY_MODULES/prompt_message_string.json'
  with open(path, "r") as file:
    message = json.load(file)

  # append message
  message.append({"role": "user", "content": str(prompt_add)})

  # Convert the list to JSON string
  message_json = json.dumps(message)

  # Write the JSON string to a file
  with open(path, "w") as file:
    file.write(message_json)
  return path

## -- ===== GPT CALL ====== ---

# Call the openAI API and generate an answer
def generate_answer(messages, temp):
    response = openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages = messages,
        max_tokens=1024,
        temperature=temp
    )
    return [response.choices[0].message.content, response.usage]

## ===== MAIN CALL FUNCTION ======
# this runs the procedure without context (initial or restart)
def run_virgin_prompt(temp):
  # generating the prompt
  prompt = hl_prompt()
  prompt_path = initial_message(prompt)
  with open(prompt_path, "r") as file:
    message = json.load(file)
  
  # calling GPT and parsing
  answer = generate_answer(message, temp)
  lines = parse_anwer(answer)
  # adjust the lenght
  
  lines1 = adjust_len(lines)
  return lines


## ===== AUXILARY CALL FUNCTION ======
# this run gets the title from the job announcement
def get_title(temp = 0.1):
  job_desc = import_job()
  prompt = f"From the following Job Description, identify the job title: {job_desc}. Return the job title without any additional comment. "
  
  message=[{"role": "user", "content": prompt}]
  answer_title = generate_answer(message, temp)
  title = parse_anwer(answer_title)
  return(title)

# gets the company name from the job desc
def get_company(temp = 0.1):
  job_desc = import_job()
  prompt = f"From the following Job Description, identify the company: {job_desc}. RETURN ONLY THE NAME OF THE COMPANY AS A STRING. If the company name is not provided, return NONE."
  message=[{"role": "user", "content": prompt}]
  answer_company = generate_answer(message, temp)
  company = parse_anwer(answer_company)
  company_clean = check_negative_return(company)
  return company_clean

# parses the return into values list
def value_analysis(temp = 0.9):
  job_desc = import_job()
  prompt = f"From the following job description, infer the 3 most important values. Answer in only 3 words: {job_desc}"
  message=[{"role": "user", "content": prompt}]
  answer_values = generate_answer(message, temp)
  val_txt = parse_anwer(answer_values)[0]
  values = val_txt.split(", ")
  return values

## ===== COLOR SEARCH ======

# Helper function to check if the string is a valid 6-character hex color
def is_valid_hex(color):
  return len(color) == 6 and all(c in '0123456789abcdefABCDEF' for c in color)

def recursive_get_color(message, temp, iteration, max_iterations):
    # Circuit breaker
    if iteration > max_iterations:
        return False
    
    # Calls GPT and parses answer
    answer_values = generate_answer(message, temp)
    val_txt = parse_anwer(answer_values)[0]
    color = val_txt.split(", ")[0]
    if is_valid_hex(color):
        return color
    else:
        return recursive_get_color(message, temp, iteration + 1, max_iterations)

# Parses the return into values list
def get_color(values, temp=1.5, max_iterations = 10):
    prompt = f"Which color is best associated with the values: {', '.join(values)}? RETURN A 6 CHARACTER HEX COLOR CODE WITHOUT ANY FURTHER INFORMATION."
    message = [{"role": "user", "content": prompt}]
    res = recursive_get_color(message = message, temp = temp, iteration = 1, max_iterations = 5)
    return [res]


## ===== LETTER CALL FUNCTION ======
# make company motivation section
def get_company_motivation(language, temp = 1):
  lang = get_lang(language)
  job_desc = import_job()
  prompt = f"From the following Job Description, write a section describing why you want this job: {job_desc}. Return a section of maximum 320 characters in {lang}."
  message=[
    {"role": "system", "content": "You are the candidate."},
    {"role": "user", "content": prompt}
    ]
  answer = generate_answer(message, temp)
  company_phrase = parse_anwer(answer)
  return company_phrase

# generate the personality traits
def get_personality(language, temp = 0.5):
  # get personality
  lang = get_lang(language)
  job_desc = import_job()
  prompt = f"From the following Job Description, identify the two most important personality traits: {job_desc}. Return two simple words in {lang}."
  message=[{"role": "user", "content": prompt}]
  answer = generate_answer(message, temp)
  personality = parse_anwer(answer)[0]
  
  # create a phrase
  style = import_txt(f"personality_style_{language}")
  prompt = f"Considering the following personality traits: '{personality}', create a phrase of the style '{style}'. Return a phrase of maximum 90 characters in {lang} without any additional comment."
  message=[{"role": "user", "content": prompt}]
  answer = generate_answer(message, 0.9)
  personality_phrase = parse_anwer(answer)

  return personality_phrase

# identifies the skills
def get_skills(language, temp = 0.9):
  lang = get_lang(language)
  # get skills
  job_desc = import_job()
  skill_set = import_txt("skill_set")
  prompt = f"""
  Job Description:  {job_desc}
  
  Skill Set: {skill_set}
  
  From the provided Job Description, identify the 3 important skills, which match the provided Skill Set. Return 3 words.
  """
  
  message=[{"role": "user", "content": prompt}]
  answer = generate_answer(message, temp)
  skills = parse_anwer(answer)[0]
  
  # create phrase
  style = import_txt(f"skill_style_{language}")
  prompt = f"Considering the following skills: '{skills}', create a phrase of the style '{style}'. Return a phrase of maximum 90 characters in {lang} without any additional comment."
  message=[{"role": "user", "content": prompt}]
  answer = generate_answer(message, 0.9)
  skill_phrase = parse_anwer(answer)
  skill_phrase

  return skill_phrase


## ===== TRANSLATE FUNCTION ======
# translates one highlight to German
def translate_highlight_german(highlight, temp = 0.1):
  prompt = f"Translate the following bullet point to German: {highlight}. If it is already in German, return it as is without any additional comment. Make sure that the result does not exceed 65 characters."
  message=[{"role": "user", "content": prompt}]
  answer_trans = generate_answer(message, temp)
  trans = parse_anwer(answer_trans)
  return trans[0]

# translates all highlights to german
def translate_all_highlights(highlights, temp = 0.1):
  for i in range(len(highlights)):
    highlights[i] = translate_highlight_german(highlights[i], temp)
  return highlights
     
     
## ===== STORES FUNCTION ======
# store the results as json
def store_results(highlights, title, company, color, values, personality_phrase, skill_phrase, company_phrase):
  # create a Dict
  result_dict = {}
  result_dict["highlight"] = {f"highlight_{i+1}": highlight for i, highlight in enumerate(highlights)}
  result_dict["title"] = title[0]
  result_dict["company"] = company[0]
  result_dict["color"] = color[0]

  result_dict["values"] = {f"value_{i+1}": value for i, value in enumerate(values)}
  result_dict["personality_phrase"] = personality_phrase[0]
  result_dict["skill_phrase"] = skill_phrase[0]
  result_dict["company_phrase"] = company_phrase[0]

  # to json
  result_json = json.dumps(result_dict)
  
  # Specify path
  return_path = 'PY_MODULES/highlight.json'

  # Write the JSON
  with open(return_path, "w") as json_file:
      json_file.write(result_json)
  return return_path


## ===== LOAD PARAMETERS TOPLINE ======

params = import_params()

openai.api_key = sys.argv[1]
prompt_add = params['prompt_add']
query_type = params['query_type']
temp = params['temp']
language = params['language']
has_letter = params['has_letter']

## ===== CV TOP LINE ======
try:
    highlights = run_virgin_prompt(temp = temp)
    if language == "de":
      highlights = translate_all_highlights(highlights)
except Exception as e:
    highlights = [False]

try:
    title = get_title( temp=0.1)
except Exception as e:
    title = [False]

try:
    company = get_company(temp=0.1)
except Exception as e:
    company = [False]

try:
    values = value_analysis(temp=0.9)
except Exception as e:
    values = [False]

try:
    color = get_color(values = values,  temp=1.5, max_iterations = 5)
except Exception as e:
    color = [False]

## ===== LETTER TOP LINE ======

if has_letter:
  try:
    personality_phrase = get_personality(language, temp = 0.5)
  except Exception as e:
    personality_phrase = [False]
  try:
    skill_phrase = get_skills(language, temp = 0.9)
  except Exception as e:
    skill_phrase = [False]
  try:
    company_phrase = get_company_motivation(language, temp = 1)
  except Exception as e:
    company_phrase = [False]
  #   
else:
  personality_phrase = [False]
  skill_phrase = [False]
  company_phrase = [False]  


## ===== JOIN AND RETURN ======
return_path = store_results(highlights, title, company, color, values, personality_phrase, skill_phrase, company_phrase)
print(return_path)
