;Templates

(deftemplate patient
    (slot age (default 0))
    (slot waist (default 0))
    (slot exercise (default 0))
    (slot BP (default 0))
    (slot food(default 0))
    (slot family(default 0))
    (slot overwt(default 0))
    (slot glucose (default 0)))

(deftemplate risk (slot score))
(deftemplate recommendation (slot risk) (slot explanation))
(deftemplate question (slot text) (slot type) (slot ident))
(deftemplate answer (slot ident) (slot text))

;Function to interact with the user to get the necessary details for evaluation

(deffunction ask-user (?question)
    "Ask a question, and return the answer"
    (printout t ?question " ")
    (return (read)))

;Module containing the rule to assert the answers given by the user to the question asked

(defmodule ask)
(defrule ask::ask-question-by-id
    "Ask a question and assert the answer"
    (declare (auto-focus TRUE))
    (MAIN::question (ident ?id) (text ?text) (type ?type))
    (not (MAIN::answer (ident ?id)))
    ?ask <- (MAIN::ask ?id)
    =>
    (bind ?answer (ask-user ?text ?type))
    (assert (MAIN::answer (ident ?id) (text ?answer)))
    (retract ?ask)
    (return))

;Startup module for the application that prints the Welcome message

(defmodule application-startup)
(defrule welcome-user
    =>
    (printout t "Welcome to the Diabetes Risk Prediction Engine!" crlf)
    (printout t "Kindly read the instructions carefully in the NOTE that follows!!" crlf)
    (printout t "Type the name of the patient and press Enter> ")
    (bind ?name (read))
    (printout t "Let us begin the Diabetes Risk evaluation for " ?name "." crlf)
    (printout t "Please provide the required information and it will tell you the risk and recommendation for the patient." crlf)
    (printout t "**NOTE** - The *AGE* of the patient must be between *20-65 YRS* and the *WAIST-LENGTH* must be greater than *65 CM*" crlf))


;Question Facts

(deffacts questions
    "The questions that are asked to the user by the system."
    (question (ident age) (type number)
        (text "What is your age ?"))
    (question (ident waist) (type number)
        (text "What is your waist length (in CM) ?"))
    (question (ident glucose) (type number)
        (text "Have you ever been diagnosed with high glucose level ? (type 0 for NO or 1 for YES)"))
    (question (ident BP) (type number)
        (text "Have you ever taken medication for high BP ? (type 0 for NO or 1 for YES)"))
    (question (ident exercise) (type number)
        (text "You do not exercise on a regular basis? (type 0 for NO or 1 for YES)"))
    (question (ident overwt) (type number)
        (text "Are you overweight (75 kg or more)? (type 0 for NO or 1 for YES)"))
    (question (ident food) (type number)
        (text "You do not eat vegetables, fruits on a regular basis ? (type 0 for NO or 1 for YES)"))
    (question (ident family) (type number)
        (text "Any family members diagnosed with diabetes previously ? (type 0 for NO or 1 for YES)")))

;Module containing rules to request the various details and assert the answers based on the different question

(defmodule request-user-details)
(defrule request-age
    =>
    (assert (ask age)))
(defrule request-waist
    =>
    (assert (ask waist)))
(defrule request-glucose
    =>
    (assert (ask glucose)))
(defrule request-BP
    =>
    (assert (ask BP)))
(defrule request-exercise
    =>
    (assert (ask exercise)))
(defrule request-overwt
    =>
    (assert (ask overwt)))
(defrule request-food
    =>
    (assert (ask food)))
(defrule request-family
    =>
    (assert (ask family)))

(defrule assert-patient-fact
    (answer (ident age) (text ?a))
    (answer (ident waist) (text ?w))
    (answer (ident glucose) (text ?g))
    (answer (ident BP) (text ?bp))
    (answer (ident exercise) (text ?e))
    (answer (ident overwt) (text ?o))
    (answer (ident food) (text ?foo))
    (answer (ident family) (text ?fam))
    =>
    (assert (patient (age ?a) (waist ?w) (glucose ?g) (BP ?bp) (exercise ?e) (overwt ?o) (food ?foo) (family ?fam))))

;Module containing rules than determine what risk and evaluation the patient would get depending on the values entered and the various combinations of these values in the answers

(defmodule risk-recommendation)
(defrule risk-group1
    (patient (age ?a&:(>= ?a 20)&:(< ?a 35))
        (waist ?w&:(>= ?w 65)&:(< ?w 85))
        (glucose ?g)
        (exercise ?e)
        (overwt ?o)
        (food ?foo)
        (family ?fam)
        (BP ?bp))
    =>
    
    (bind ?calculated-risk (integer (+ (+ 2) (* 5 ?g) (* 5 ?bp) (* 5 ?foo) (* 5 ?o) (* 5 ?e) (* 5 ?fam))))
    (if(> ?calculated-risk 40) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a very high risk of diabetes! Consult a doctor immediately!! Consult a doctor immediately!!")))
        elif(> ?calculated-risk 20) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have an average risk of diabetes! You might want to see a doctor to make sure! You might want to see a doctor to make sure!")))
        else
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a low risk of diabetes! Congratulations, you are healthy! Congratulations, you are healthy!")))))


(defrule risk-group2
    (patient (age ?a&:(>= ?a 20)&:(< ?a 35))
        (waist ?w&:(>= ?w 85)&:(< ?w 105))
        (glucose ?g)
        (exercise ?e)
        (overwt ?o)
        (food ?foo)
        (family ?fam)
        (BP ?bp))
    =>
    
    (bind ?calculated-risk (integer (+ (+ 4) (* 5 ?g) (* 5 ?bp) (* 5 ?foo) (* 5 ?o) (* 5 ?e) (* 5 ?fam))))
    (if(> ?calculated-risk 40) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a very high risk of diabetes! Consult a doctor immediately!!")))
        elif(> ?calculated-risk 20) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have an average risk of diabetes! You might want to see a doctor to make sure!")))
        else
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a low risk of diabetes! Congratulations, you are healthy!")))))


(defrule risk-group3
    (patient (age ?a&:(>= ?a 35)&:(< ?a 45))
        (waist ?w&:(>= ?w 65)&:(< ?w 85))
        (glucose ?g)
        (exercise ?e)
        (overwt ?o)
        (food ?foo)
        (family ?fam)
        (BP ?bp))
    =>
    
    (bind ?calculated-risk (integer (+ (+ 4) (* 5 ?g) (* 5 ?bp) (* 5 ?foo) (* 5 ?o) (* 5 ?e) (* 5 ?fam))))
    (if(> ?calculated-risk 40) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a very high risk of diabetes! Consult a doctor immediately!!")))
        elif(> ?calculated-risk 20) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have an average risk of diabetes! You might want to see a doctor to make sure!")))
        else
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a low risk of diabetes! Congratulations, you are healthy!")))))

(defrule risk-group4
    (patient (age ?a&:(>= ?a 35)&:(< ?a 45))
        (waist ?w&:(>= ?w 85)&:(< ?w 105))
        (glucose ?g)
        (exercise ?e)
        (overwt ?o)
        (food ?foo)
        (family ?fam)
        (BP ?bp))
    =>
    
    (bind ?calculated-risk (integer (+ (+ 7) (* 5 ?g) (* 5 ?bp) (* 5 ?foo) (* 5 ?o) (* 5 ?e) (* 5 ?fam))))
    (if(> ?calculated-risk 40) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a very high risk of diabetes! Consult a doctor immediately!!")))
        elif(> ?calculated-risk 20) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have an average risk of diabetes! You might want to see a doctor to make sure!")))
        else
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a low risk of diabetes! Congratulations, you are healthy!")))))


(defrule risk-group5
    (patient (age ?a&:(>= ?a 45)&:(< ?a 55))
        (waist ?w&:(>= ?w 65)&:(< ?w 85))
        (glucose ?g)
        (exercise ?e)
        (overwt ?o)
        (food ?foo)
        (family ?fam)
        (BP ?bp))
    =>
    
    (bind ?calculated-risk (integer (+ (+ 6) (* 5 ?g) (* 5 ?bp) (* 5 ?foo) (* 5 ?o) (* 5 ?e) (* 5 ?fam))))
    (if(> ?calculated-risk 40) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a very high risk of diabetes! Consult a doctor immediately!!")))
        elif(> ?calculated-risk 20) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have an average risk of diabetes! You might want to see a doctor to make sure!")))
        else
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a low risk of diabetes! Congratulations, you are healthy!")))))

(defrule risk-group6
    (patient (age ?a&:(>= ?a 45)&:(< ?a 55))
        (waist ?w&:(>= ?w 85)&:(< ?w 105))
        (glucose ?g)
        (exercise ?e)
        (overwt ?o)
        (food ?foo)
        (family ?fam)
        (BP ?bp))
    =>
    
    (bind ?calculated-risk (integer (+ (+ 9) (* 5 ?g) (* 5 ?bp) (* 5 ?foo) (* 5 ?o) (* 5 ?e) (* 5 ?fam))))
    (if(> ?calculated-risk 40) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a very high risk of diabetes! Consult a doctor immediately!!")))
        elif(> ?calculated-risk 20) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have an average risk of diabetes! You might want to see a doctor to make sure!")))
        else
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a low risk of diabetes! Congratulations, you are healthy!")))))

(defrule risk-group7
    (patient (age ?a&:(>= ?a 55)&:(< ?a 65))
        (waist ?w&:(>= ?w 65)&:(< ?w 85))
        (glucose ?g)
        (exercise ?e)
        (overwt ?o)
        (food ?foo)
        (family ?fam)
        (BP ?bp))
    =>
    
    (bind ?calculated-risk (integer (+ (+ 11) (* 5 ?g) (* 5 ?bp) (* 5 ?foo) (* 5 ?o) (* 5 ?e) (* 5 ?fam))))
    (if(> ?calculated-risk 40) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a very high risk of diabetes! Consult a doctor immediately!!")))
        elif(> ?calculated-risk 20) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have an average risk of diabetes! You might want to see a doctor to make sure!")))
        else
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a low risk of diabetes! Congratulations, you are healthy!")))))

(defrule risk-group8
    (patient (age ?a&:(>= ?a 55)&:(< ?a 65))
        (waist ?w&:(>= ?w 85)&:(< ?w 105))
        (glucose ?g)
        (exercise ?e)
        (overwt ?o)
        (food ?foo)
        (family ?fam)
        (BP ?bp))
    =>
    
    (bind ?calculated-risk (integer (+ (+ 18) (* 5 ?g) (* 5 ?bp) (* 5 ?foo) (* 5 ?o) (* 5 ?e) (* 5 ?fam))))
    (if(> ?calculated-risk 40) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a very high risk of diabetes! Consult a doctor immediately!!")))
        elif(> ?calculated-risk 20) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have an average risk of diabetes! You might want to see a doctor to make sure!")))
        else
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a low risk of diabetes! Congratulations, you are healthy!")))))

(defrule risk-group9
    (patient (age ?a&:(>= ?a 20)&:(< ?a 35))
        (waist ?w&:(>= ?w 105))
        (glucose ?g)
        (exercise ?e)
        (overwt ?o)
        (food ?foo)
        (family ?fam)
        (BP ?bp))
    =>
    
    (bind ?calculated-risk (integer (+ (+ 11) (* 5 ?g) (* 5 ?bp) (* 5 ?foo) (* 5 ?o) (* 5 ?e) (* 5 ?fam))))
    (if(> ?calculated-risk 40) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a very high risk of diabetes! Consult a doctor immediately!!")))
        elif(> ?calculated-risk 20) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have an average risk of diabetes! You might want to see a doctor to make sure!")))
        else
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a low risk of diabetes! Congratulations, you are healthy!")))))

(defrule risk-group10
    (patient (age ?a&:(>= ?a 35)&:(< ?a 45))
        (waist ?w&:(>= ?w 105))
        (glucose ?g)
        (exercise ?e)
        (overwt ?o)
        (food ?foo)
        (family ?fam)
        (BP ?bp))
    =>
    
    (bind ?calculated-risk (integer (+ (+ 14) (* 5 ?g) (* 5 ?bp) (* 5 ?foo) (* 5 ?o) (* 5 ?e) (* 5 ?fam))))
    (if(> ?calculated-risk 40) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a very high risk of diabetes! Consult a doctor immediately!!")))
        elif(> ?calculated-risk 20) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have an average risk of diabetes! You might want to see a doctor to make sure!")))
        else
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a low risk of diabetes! Congratulations, you are healthy!")))))

(defrule risk-group11
    (patient (age ?a&:(>= ?a 45)&:(< ?a 55))
        (waist ?w&:(>= ?w 105))
        (glucose ?g)
        (exercise ?e)
        (overwt ?o)
        (food ?foo)
        (family ?fam)
        (BP ?bp))
    =>
    
    (bind ?calculated-risk (integer (+ (+ 19) (* 5 ?g) (* 5 ?bp) (* 5 ?foo) (* 5 ?o) (* 5 ?e) (* 5 ?fam))))
    (if(> ?calculated-risk 40) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a very high risk of diabetes! Consult a doctor immediately!!")))
        elif(> ?calculated-risk 20) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have an average risk of diabetes! You might want to see a doctor to make sure!")))
        else
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a low risk of diabetes! Congratulations, you are healthy!")))))

(defrule risk-group12
    (patient (age ?a&:(>= ?a 55)&:(< ?a 65))
        (waist ?w&:(>= ?w 105))
        (glucose ?g)
        (exercise ?e)
        (overwt ?o)
        (food ?foo)
        (family ?fam)
        (BP ?bp))
    =>
    
    (bind ?calculated-risk (integer (+ (+ 27) (* 5 ?g) (* 5 ?bp) (* 5 ?foo) (* 5 ?o) (* 5 ?e) (* 5 ?fam))))
    (if(> ?calculated-risk 40) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a very high risk of diabetes! Consult a doctor immediately!!")))
        elif(> ?calculated-risk 20) then
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have an average risk of diabetes! You might want to see a doctor to make sure!")))
        else
        (assert (recommendation
                (risk ?calculated-risk)
                (explanation "You have a low risk of diabetes! Congratulations, you are healthy!")))))

;Module that contains the rules to print out the final result of the evaluation

(defmodule result)
(defrule print-result
    ?p1 <- (recommendation (risk ?r1) (explanation ?e))
    =>
    (printout t "*** The risk for this player is :" ?r1 crlf)
    (printout t "Explanation: " ?e crlf crlf))

;Function to run the various modules of the application in the correct order

(deffunction run-application ()
    (reset)
    (focus application-startup request-user-details risk-recommendation result)
    (run))


(run-application)
