<?php
function CheckLogin(){
  if(!isset($_COOKIE['rwo_user'])){
    header('Location: login.php');
  }
}

function LogAccess($filename){
    if(isset($_COOKIE['rwo_user'])){
      $handle = fopen('accesslog.txt','a');
      $user = $_COOKIE['rwo_user'];
      $time = date("Y-m-d G:i:s", time());
      fwrite($handle,"$user - $time - $filename \n");
      fclose($handle);
    }
}

?>
