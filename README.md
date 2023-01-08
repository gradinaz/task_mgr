# task_mgr

Build
-----

    $ rebar3 compile
    
Run
-----

    $ rebar3 shell 
   
Test
-----
You can use *curl* to test this web server.
To get ordered tasks:
  

       curl --header "Content-Type: application/json" --data @doc/tasks.json http://localhost:8084/api/tasks
     
Output: 

     [
        {
          "command": "touch \/tmp\/file1",
          "name": "task-1"
        },
        {
          "command": "echo 'Hello World!' > \/tmp\/file1",
          "name": "task-3"
        },
        {
          "command": "cat \/tmp\/file1",
          "name": "task-2"
        },
        {
          "command": "rm \/tmp\/file1",
          "name": "task-4"
        }
      ]

To get bash script body: 

        curl --header "Content-Type: application/json" --data @doc/tasks_bash.json http://localhost:8084/api/tasks
        
Output: 

      "#!\/usr\/bin\/env_bash\
      rm \/tmp\/file1\
      cat \/tmp\/file1\
      echo 'Hello World!' > \/tmp\/file1\
      touch \/tmp\/file1"

      
