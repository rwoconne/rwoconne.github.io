<?php

include('rwo_functions.php');



    if($_POST['submit_check'] == '1'){

    $username = $_POST['username'];

    $password = $_POST['password'];



    $lines = file('/www/grades/passwds');

    foreach ($lines as $line_num => $line) {

        $pieces = explode("	",$line,8);

          if($pieces[0] == $username){

            $pieces[1] = trim($pieces[1]);

            if($pieces[1] == $password){

                setcookie('rwo_user',$username);

                header('Location: lectureindex.html');

            }

          }

            else

                $error = 'Incorrect password';

    }

    if(!isset($error))

        $error = 'Invalid login. If you do not have an account on the system you can get one <a href="http://www.astro.virginia.edu/class/grade-login.html">here</a>';

    }

?>

<html>

    <head>

    <title></title>

    </head>

    <body>

Please log in with your password to the astronomy grade system.

        <?php echo $error . "<br>\n";?>



        <form method="post" action="login.php">

        <input type="hidden" name="submit_check" value="1">

        Username: <input name="username" size="10"><br>

        Password: <input type="password" name="password" size="10"><br>

        <input type="submit" value="Login">

        </form>



    </body>

</html>

