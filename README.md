# Naive Bayes for Sentiment Analysis

### Group members:
- Konstantin
- Yegor 
---

## Project Background

The [Bayes' theorem](https://en.wikipedia.org/wiki/Bayes%27_theorem) is a simple yet powerful mathematical formula that allows predictions to be made based on prior beliefs and the likelihood of evidence observed.

<img src="/resources/bayes_formula_dark.svg#gh-light-mode-only" width="220px" alt="Bayes' theorem'">
<img src="/resources/bayes_formula_light.svg#gh-dark-mode-only" width="220px" alt="Bayes' theorem'">
 
- $A, B = \text{events} $
- $P(A) = \text{independent probability of A}$
- $P(B) = \text{independent probability of B}$
- $P(A|B) = \text{probability of A given B is true}$
- $P(B|A) = \text{probability of B given A is true}$

A [Naive Bayes classifier](https://en.wikipedia.org/wiki/Naive_Bayes_classifier) is in the family of probabilistic classifiers that is used among other things in tasks such as: [spam prediction](https://en.wikipedia.org/wiki/Naive_Bayes_spam_filtering#:~:text=Naive%20Bayes%20spam%20filtering%20is,with%20roots%20in%20the%201990s), sentiment analysis, recommendation systems.

## What problem does our program solve?
Here we will be using the algorithm for sentiment analysis for movie reviews to classify them as either a positive or a negative review.

Using [Haskell](https://wiki.haskell.org/Haskell), our program will:
- perform **text preprocessing** on text data.
  - remove stop words (words that don't add much value to the text).
  - remove symbols/punctuation.
- train and test the model using a publicly available dataset[^1] - the [IMDb dataset](https://ai.stanford.edu/~amaas/data/sentiment/) contains 25,000 positive and 25,000 negative movie reviews collected from [IMDb](https://en.wikipedia.org/wiki/IMDb).
  - although the user can use our application on any other data, as long as it is added as a txt file in the correct folder within the *datasets* directory.
- perform validation of a trained model using test data.
- implement a command-line interface where users can input strings of reviews and get the model's inference result.

## What is the something extra?

In addition to the expected basic functionality, our program will:
- allow users to download the model parameters in a text file.
- re-train the model with various dataset sizes and compare their performance.

## What did we learn from doing this?

We acquired more practical experience in writing Haskell applications and Cabal projects. We learned a more in-depth about Input/Output, several data structures and their operations (Map,Set, etc). We also acquired a better intuition about cryptic Haskell compile errors and how to debug them. Also, not the least important, we got a taste of working with files, like reading the data to and from them. Finally, by implementing the simple Naive-Bayes algorithm we saw the potential in implementing machine learning algorithms in Haskell.

---

## How to use this program?

Assuming all imports are working with cabal (which is unlikely since we haven't tested it and we are sure there are problems), just do:
1. Go into /app folder
2. Type `ghci`
3. Type `:l Main`
4. Type `main` to start the Interactive CLI

How to use the CLI?
- Once your program is running with `main`, the `help` command gives all of the commands that you need to know
- You can exit by typing `exit` or `e`
- You can get the list of all models in the /models folder by typing `models` or `m`
- You can select/load a model from that list using `load <model_name`
- You can see the currently selected model and some info about it using `state` or `s`
- You can train a new model using `train <model_name>`. If a duplicate name is given, it overwrites that model I believe
- You can validate a model using `validate` or `v`, it always validates the currently selected model (we didn't have time to implement more functions so bear with us)

## What does the program do?
When doing training, the program is able to generate new training and testing sets, since it shuffles the file lists randomly.
You can select the training split, we made 3 options:
- 25%-75% train-validation %
- 50%-50% train-validation %
- 75%-25% train-validation %

---
- You don't need to validate right away with our program, since we save the test data sets for later use :)
- The program does not re-validate a model that was previously validated, as long as the its score can be found in scores.json 
  - This saves a lot of time for unnecessary recomputation

[^1]: dataset citation, see [also](https://github.com/yegory/NaiveBayes/tree/master/datasets): 
@InProceedings{maas-EtAl:2011:ACL-HLT2011,
  author    = {Maas, Andrew L.  and  Daly, Raymond E.  and  Pham, Peter T.  and  Huang, Dan  and  Ng, Andrew Y.  and  Potts, Christopher},
  title     = {Learning Word Vectors for Sentiment Analysis},
  booktitle = {Proceedings of the 49th Annual Meeting of the Association for Computational Linguistics: Human Language Technologies},
  month     = {June},
  year      = {2011},
  address   = {Portland, Oregon, USA},
  publisher = {Association for Computational Linguistics},
  pages     = {142--150},
  url       = {http://www.aclweb.org/anthology/P11-1015 }
}
