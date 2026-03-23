120 LOAD
ONLY FORTH DEFINITIONS  ALSO SYSTEM

CR .( MultiTasking tests )
CR .( Task "t1" updates task-count in the background. )
CR .( Use .t1 to output current value. )
CR .( Task "t2" outputs "hello!" and then ends )
CR .( Use t2 WAKE-TASK to retrigger the task )
CR .( Task "t3" outputs 4 digits then puts itself to sleep )
CR .( Use t3 WAKE-TASK to repeat )

CR .( Use MULTI to start multitasking, and SINGLE to stop )
CR

1 +LOAD
PREVIOUS
VARIABLE task-count ;
: .t1  task-count ATOMIC@ . ;
: task1 
   0 task-count ! BEGIN  
      1 task-count ATOMIC+!
      PAUSE
   AGAIN ;

: task2 ." hello!" CR  ;
: task3 BEGIN  4 0 DO I U. PAUSE LOOP SLEEP AGAIN ;

TASK t1    ' task1 t1 START-TASK
TASK t2    ' task2 t2 START-TASK
TASK t3    ' task3 t3 START-TASK
