#====Using ChatGPT and Github copilot====
# Chatgpt in rstudio is recommended for coding explanation, debug and general knowledge
# Github copilot is recommended for real time code suggestion with ghost text, autocomplete style with proper sequence
# With github education eligible for github copilot free subscription, you can use copilot in Rstudio

#====Using chattr for link chatgpt in rstudio====
install.packages("chattr")
library(chattr)
library(shiny)

Sys.setenv("OPENAI_API_KEY" = "put your api here") 
#get you free api key from open ai,Chattr will not properly installed or authenticated if you do not authenticate your API keys
#get your api from https://platform.openai.com/account/api-keys

chattr_use("gpt4o") #select model 4o
chattr_app(as_job = FALSE)
#click addins, select open chat
#click the stop button of the application everytime finish chat and to resume running code
#close terminal once finish 
#example how to generate R scripts with proper prompt
#====Generate an R script code =====
# Example chatgpt prompt is : Import dataset from https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/refs/heads/main/epidemic/linelist/linelist_deaths.csv into deaths dataset
deaths <- read_csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/linelist/linelist_deaths.csv")

# Example copilot prompt is : summarize deaths in table summary using dplyr and gtsummary
deaths_summary <- deaths %>%
  tbl_summary(missing = "always") %>%
  modify_caption("**Initial Summary of COVID-19 Linelist Deaths Dataset**") %>%
  bold_labels()
#print the result
print(deaths_summary)

# Example prompt is : Generate an R script code that run chi square using arsenal package using deaths dataset between bid variable and vax_status variable
chi_square_result <- tableby(deaths$bid ~ deaths$vax_status, 
                             data = deaths, 
                             stats = c("countpct", "chisq"))
summary(chi_square_result)


#====Debugging Rscript syntax issue====
#Please help debug this R script.  The error I'm getting is: 'Error: unexpected symbol in: ")'. Here's the code:"
#fit_bid < glm(bid ~ vax_status + age_group + gender + comorb + state, data = deaths, family = "binomial
#====Exploring alternative approaches====
#"My current script uses piping (%>%). Can you rewrite the same logic without using the pipe operator?". Here's the code: "fit_bid %>% tbl_regression(exp = TRUE) %>% modify_caption("Logistic Regression Predicting BID Status") %>% modify_header( label = "Variable", estimate = "OR", conf.low = "LL 95%CI", conf.high = "UL 95%CI", p.value = "p-value" ) %>% bold_labels() "
tbl_result <- tbl_regression(fit_bid, exp = TRUE)
tbl_result <- modify_caption(tbl_result, "**Logistic Regression Predicting BID Status**")
tbl_result <- modify_header(
  tbl_result,
  label = "**Variable**",
  estimate = "**OR**",
  conf.low = "**LL 95%CI**",
  conf.high = "**UL 95%CI**",
  p.value = "**p-value**"
)
tbl_result <- bold_labels(tbl_result)
tbl_result


#====Using Github copilot ====

#Copilot offers autocomplete-style suggestions as you code as “ghost text”
#This ghost-text is similar to the existing autocomplete available in RStudio but importantly is a generated suggestion rather than an exact auto-completion.


#1.Navigate to Tools > Global Options > Copilot.
#2.Check the box to “Enable GitHub Copilot”.
#3.Download and install the Copilot Agent components.
#4.Click the “Sign In” button.
#5.In the “GitHub Copilot: Sign in” dialog, copy the Verification Code.
#6.Navigate to or click on the link to https://github.com/login/device, paste the Verification Code and click “Continue”.
#7.GitHub will request the necessary permissions for GitHub Copilot. To approve these permissions, click “Authorize GitHub Copilot Plugin”.
#8.After the permissions have been approved, your RStudio IDE will indicate the currently signed in user.
#9.Close the Global Options dialogue, open a source file (.R, .py, .qmd, etc) and begin coding with Copilot!
#10.To disable GitHub Copilot Navigate to Tools > Global Options > Copilot and uncheck “Enable GitHub Copilot”, or
#11 (press tab to use the full suggestion)
#Example 1 bold_labels
covid_url <- "https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/refs/heads/main/epidemic/linelist/linelist_deaths.csv"
deaths <- read_csv(covid_url)

# Initial exploration (summary + missing)
deaths %>%
  tbl_summary(missing = "always") %>%
  modify_caption("**Initial Summary of COVID-19 Linelist Deaths Dataset**") %>%
  b

#Example 2 - Plotting mortality rate by vaccination status

#Example 3- Compute max y for placing labels

#====Using Deepseek====

##Prerequisite 1. Hugging face account
#sign up hugging face account https://huggingface.co/welcome

##Prerequisite 2. Plumber package to connect API endpoint 
install.packages("plumber")
library(plumber)

##Prerequisite 3. Httr package to work with http request and interact with huggingface api
install.packages("httr2")
library(httr2)

##Step 1 Create project 
#create new project - new plumber api project
#clear plumber.R file that is filled with sample api
#go to terminal and create .env file which will store your api
#create .gitignore file in git tab so that api and token will not be push in git repository
##If you dont have the Git tab, click tools > version control > project setup > git/svn > version control git
##Go to git pane (the tab that says Git)
#Right click on .Rhistory and select Ignore...
#Check if the content of .gitignore is correctly updated and click Save.
#open the .gitignore file add .env and save

##Step 2 Create huggingface access token
#create hugging face token at https://huggingface.co/settings/tokens
#copy and paste the token in the .env file
#HUGGINGFACE_ACCESS_TOKEN="<your token>"
#install dotenv package 
install.packages("dotenv")
library(dotenv)
# Copy this in plumber.R file. 
# Load environment variables from .env
dotenv::load_dot_env()

##Step 3 Build deepseek api endpoint 
#go to plumber.R file and load plumber and http package
# api_key <- Sys.getenv("HUGGINGFACE_ACCESS_TOKEN")
# and paste this code
#* @post /deepseek_chat
function(prompt) {
  url <- "https://huggingface.co/api/inference-proxy/together/v1/chat/completions"
  
  # Create a request object
  req <- request(url) |>
    req_auth_bearer_token(api_key) |>
    req_body_json(list(
      model = "deepseek-ai/DeepSeek-R1",
      messages = list(
        list(role = "user", content = prompt)
      ),
      max_tokens = 500,
      stream = FALSE
    ))
  
  # Perform the request and capture the response
  res <- req_perform(req)
  
  # Parse the JSON response
  parsed_data <- res |>
    resp_body_json()
  
  # Extract the content from the response
  content <- parsed_data$choices
  return(content)
}

## Step 4 Test the API 
#Let’s run the API endpoint to see how the application performs.
#Click on Run API. 
#This will automatically open the API endpoint on your browser on the URL http://127.0.0.1:8634/docs/.

#====Key Ethical points====

#Objective: Highlight the responsible use of AI in public health and transparency reporting based on current guideline. 
#Key Points:
#1.	Benefits and limitations of AI tools like ChatGPT.
#2.	Ensuring data privacy and avoiding bias in analyses.
#3.	The importance of human oversight in AI-generated outputs.
#4.	Transparency reporting of using AI.


#1. Benefits and Limitations of AI Tools Like ChatGPT
#AI tools can enhance productivity, improve data analysis, and support health communication. ChatGPT, for instance, can assist in summarizing research, drafting reports, and generating health education content.
#However, limitations include potential inaccuracies, hallucinated outputs, and lack of contextual awareness. AI models may also reflect biases present in their training data.

#2. Ensuring Data Privacy and Avoiding Bias
#Ethical AI use must comply with data protection regulations (e.g., HIPAA, GDPR). Sensitive or identifiable health data should never be input into generative AI tools.
#Bias in AI outputs must be mitigated by critically assessing data sources, ensuring diverse training inputs, and involving domain experts in evaluation processes.

#3. Importance of Human Oversight
#AI-generated content should be reviewed and validated by qualified professionals. Final decisions—especially in clinical or public health contexts—must remain under human authority to maintain accountability and context-appropriate judgment.

#4. Transparency in AI Use Reporting
#When AI tools contribute to research or public health documentation, their use must be clearly disclosed. This includes:

#Stating the name and version of the tool (e.g., OpenAI ChatGPT, GPT-4).
#Describing how AI was used (e.g., content generation, data analysis assistance).
#Clarifying that outputs were reviewed and edited by humans.

#Example statement for research reporting:
# “Portions of this manuscript were generated with the assistance of ChatGPT (OpenAI), and subsequently reviewed and edited by the authors for accuracy and context.”

#Local References 
#1. https://mmc.gov.my/wp-content/uploads/2025/04/Ethical-Use-of-Artificial-Intelligence-in-Medical-Practice.pdf