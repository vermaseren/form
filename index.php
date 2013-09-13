<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="utf-8">
        <title>FORM</title>
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta name="description" content="">
        <meta name="author" content="Ben Ruijl">

        <link href="css/bootstrap.css" rel="stylesheet">
        <style type="text/css">
            body {
                padding-top: 60px;
                padding-bottom: 40px;
            }
        </style>
        <link href="css/bootstrap-responsive.css" rel="stylesheet">

        <!-- HTML5 shim, for IE6-8 support of HTML5 elements -->
        <!--[if lt IE 9]>
          <script src="js/html5shiv.js"></script>
        <![endif]-->

        <!-- Fav and touch icons -->
        <link rel="apple-touch-icon-precomposed" sizes="144x144" href="ico/apple-touch-icon-144-precomposed.png">
        <link rel="apple-touch-icon-precomposed" sizes="114x114" href="ico/apple-touch-icon-114-precomposed.png">
        <link rel="apple-touch-icon-precomposed" sizes="72x72" href="ico/apple-touch-icon-72-precomposed.png">
        <link rel="apple-touch-icon-precomposed" href="ico/apple-touch-icon-57-precomposed.png">
        <link rel="shortcut icon" href="ico/favicon.png">

        <script src="js/jquery-2.0.3.min"></script>
        <script src="js/bootstrap.min.js"></script>
    </head>

    <body>

        <div class="navbar navbar-inverse navbar-fixed-top">
            <div class="navbar-inner">
                <div class="container">
                    <button type="button" class="btn btn-navbar" data-toggle="collapse" data-target=".nav-collapse">
                        <span class="icon-bar"></span>
                        <span class="icon-bar"></span>
                        <span class="icon-bar"></span>
                    </button>
                    <a class="brand" href="?page=intro">FORM</a>
                    <div class="nav-collapse collapse">
                        <ul class="nav">
                            <li class="active"><a href="?page=intro">Home</a></li>
                            <li><a href="?page=features">Features</a></li>
                            <li><a href="?page=doc">Documentation</a></li>
                            <li><a href="?page=contact">Contact</a></li>
                        </ul>
                    </div><!--/.nav-collapse -->
                </div>
            </div>
        </div>

        <div style="position:absolute;right:0px;top:40px"><a href="https://github.com/vermaseren/form"><img src="https://s3.amazonaws.com/github/ribbons/forkme_right_green_007200.png" alt="Fork me on GitHub"></a></div>
        <div class="container">

            <?php
            $active_page = $_GET['page'];
            
            if (empty($active_page)) {
                $active_page = 'intro';
            }
            
            require_once($active_page . '.php'); // FIXME: not safe
            ?>

            <hr>

            <footer>
                <p>FORM &copy; Jos Vermaseren 2013</p>
            </footer>

        </div>

    </body>
</html>
