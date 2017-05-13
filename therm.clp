(deftemplate rule 
   (multislot if)
   (multislot then))


(defrule system-banner ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "RULE-BASED EXPERT SYSTEM - THERMOSTAT")
  (printout t crlf crlf))

(defrule propagate-goal ""
   (goal is ?goal)
   (rule (if ?variable $?)
         (then ?goal ? ?value))
   =>
   (assert (goal is ?variable)))

(defrule goal-satified ""
   (declare (salience 30))
   ?f <- (goal is ?goal)
   (variable ?goal ?value)
   (answer ? ?text ?goal)
   =>
   (retract ?f)
   (format t "%s%s%n" ?text ?value))

(defrule remove-rule-no-match ""
   (declare (salience 20))
   (variable ?variable ?value)
   ?f <- (rule (if ?variable ? ~?value $?))
   =>
   (retract ?f))

(defrule modify-rule-match ""
   (declare (salience 20))
   (variable ?variable ?value)
   ?f <- (rule (if ?variable ? ?value and $?rest))
   =>
   (modify ?f (if ?rest)))

(defrule rule-satisfied ""
   (declare (salience 20))
   (variable ?variable ?value)
   ?f <- (rule (if ?variable ? ?value)
               (then ?goal ? ?goal-value))
   =>
   (retract ?f)
   (assert (variable ?goal ?goal-value)))

(defrule ask-question-no-legalvalues ""
   (declare (salience 10))
   (not (legalanswers $?))
   ?f1 <- (goal is ?variable)
   ?f2 <- (question ?variable ? ?text and legalanswers ? $?answers where hints ? $?msg)
   =>
   (retract ?f1 ?f2)
   (format t "%s " ?text)
   (assert (variable ?variable (read))))

(defrule ask-question-legalvalues ""
   (declare (salience 10))
   ?f1 <- (goal is ?variable)
   ?f2 <- (question ?variable ? ?text and legalanswers ? $?answers where hints ? $?msg)
   =>
   (retract ?f1)      
   (format t "%s " ?text)
   (printout t "LEGAL ANSWERS ARE :: " ?answers " ")
   (printout t crlf crlf)
   (printout t "====================EXPLANATION====================" crlf)
   (printout t ?msg crlf "")
   (printout t "===================================================" crlf)
   (printout t crlf crlf)
   
   (bind ?reply (read))
   (printout t crlf crlf)
   (if (member (lowcase ?reply) ?answers) 
     then (assert (variable ?variable ?reply))
          (retract ?f2)
     else (assert (goal is ?variable))))



(deffacts knowledge-base 
   (goal is type.degree)

   (rule (if month is dec) 
         (then season is summer))
   (rule (if month is jan) 
         (then season is summer))
   (rule (if month is feb) 
         (then season is summer))
   (rule (if month is mar) 
         (then season is autumn))
   (rule (if month is apr) 
         (then season is autumn))
   (rule (if month is may) 
         (then season is autumn))
   (rule (if month is jun) 
         (then season is winter))
   (rule (if month is jul) 
         (then season is winter))
   (rule (if month is aug) 
         (then season is winter))
   (rule (if month is sep) 
         (then season is spring))
   (rule (if month is oct) 
         (then season is spring))
   (rule (if month is nov) 
         (then season is spring))
   (question month is "What month is it?" and legalanswers are jan feb mar apr may jun jul aug sep oct nov dec
    where hints are jan stands for JANUARY feb stands for FEBRUARY and so an)      
   
   (rule (if day is mon) 
         (then today is workday))
   (rule (if day is tue) 
         (then today is workday))
   (rule (if day is wed) 
         (then today is workday))
   (rule (if day is thu) 
         (then today is workday))
   (rule (if day is fri) 
         (then today is workday))
   (rule (if day is sat) 
         (then today is weekend))
   (rule (if day is sun) 
         (then today is weekend))
   

   (question day is "What day is it?" and legalanswers are sat sun mon tue wed thu fri 
    where hints are sat stands for SATURDAY sun stands for SUNDAY and so an)

   (rule (if today is weekend) 
         (then operation is notDuringBusinessHour))   
   (rule (if today is workday and operation.time is opt1) 
         (then operation is duringBusinessHour))      
   (rule (if today is workday and operation.time is opt2) 
         (then operation is notDuringBusinessHour))
   (rule (if today is workday and operation.time is opt3) 
         (then operation is notDuringBusinessHour))
   (question operation.time is "What time of the day is it?" and legalanswers are opt1 opt2 opt3
    where hints are opt1 stands for "BETWEEN 9 AM AND 5PM" ,opt2 stands for "BEFORE 9 AM" and ,opt3 stands for "AFTER 5 PM")
   
   (rule (if season in spring and operation is duringBusinessHour)
         (then type.degree is 20-degree))
   (rule (if season in spring and operation is notDuringBusinessHour)
         (then type.degree is 15-degree))
   (rule (if season in summer and operation is duringBusinessHour)
         (then type.degree is 24-degree))
   (rule (if season in summer and operation is notDuringBusinessHour)
         (then type.degree is 27-degree))
   (rule (if season in autumn and operation is duringBusinessHour)
         (then type.degree is 20-degree))
   (rule (if season in autumn and operation is notDuringBusinessHour)
         (then type.degree is 16-degree))
   (rule (if season in winter and operation is duringBusinessHour)
         (then type.degree is 16-degree))
   (rule (if season in winter and operation is notDuringBusinessHour)
         (then type.degree is 18-degree))
 
   (answer is "I think your thermostat setting is " type.degree))
