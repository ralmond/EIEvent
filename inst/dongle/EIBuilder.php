<?php

if ($_SERVER['REQUEST_METHOD'] == 'GET' || $_SERVER['REQUEST_METHOD'] == 'POST' ) {
?>
<html>
<head><title>Evidence Identification Rule Set Builder</title></head>
<body>

    <p> This is an interface to the Eevidence Identification process
    of the Proc4 system.  For more information about Proc4, go to
        <a href="https://pluto.coe.fsu.edu/Proc4/">Proc 4 home
            page.</a></p>

    <p>This script rebuilds the evidnece rules loading them from a
        preconfigured github repository.  This works according to the
        following steps:</p>
    <ol>
        <li>The latest configuration information and rules are pulled down from
            <a href="https://github.com/ralmond/PP-EI">github</a>.
        The branch listed in the field below is the one which will be
        checked out. </li> 

        <li>The <code>config.json</code> file is read to pick up the details.</li>

        <li>The rules files from github are loaded into the
            database.</li>
    </ol>


    <form action="<?php echo $_SERVER['PHP_SELF'] ?>" method="POST">
        App: <input type="url" id="app" name="app" pattern="ecd://.*" required/><br/>
        Branch: <input id="branch" name="Version branch" value="PP-main" required/>
        <br/>
        Administrator: <input type="text" id="aid" name="aid"/ required><br/>
        Password: <input type="password" id="pwd" name="pwd"/ required><br/>
        <input type="submit" name="Do it!"/>
    </form>
    <?php
    if ($_SERVER['REQUEST_METHOD'] == 'POST')
    {
        include 'config.php';
        include 'checkPwd.php';

        $app = $_POST['app'];
        if (strpos($app,'ecd://epls.coe.fsu.edu/') != 0) {
            die("That application is not supported on this server.");
        }

        if (!in_array($app,$INI['apps'])) {
            die("App '$app' not registered.");
        }
        $aid = $_POST['aid'];
        $pwd = $_POST['pwd'];
        $filepwd = get_htpasswd('/usr/local/share/Proc4/p4pwds',$aid);
        if (!matches($pwd,$filepwd)) {
            die("Username or password not matched.");
        }
        $branch = $_POST['branch'];
        $EIhome = $INI['config']['EIHome'];
        $sapp = basename($app);
    ?>
    <h2>Updating configuration <?php echo $branch ?> from github</h2>
    <pre><?php `/usr/local/share/bin/gitconfig $EIhome $branch` ?>
    </pre><br/>
    <?php
    $configfile = file_get_contents($EIhome."/config.json");
    $EIconfig = json_decode($configfile);
    /* Check for Rule Directory */
    $ruledir = $EIconfig['ruledir']
    if (is_null($ruledir)) {
        $ruledir = "Rules";
    }
    $ruledir = $EIhome."/".$ruledir;
    if (!file_exists($ruledir)) {
        die("Can't find rule directory.")
    }

    $logdir = $INI['config']['logdir'];
    $logfile = $logdir."/".str_replace("<app>",$sapp,$EIconfig['logname']);
    $lockfile = $ruledir."/"."ruleloader.lock";

    if (file_exists($lockfile)) {
        die("Rule load already in progress.");
    }
    if (file_exists($logfile)) {
        unlink($logfile)
    }
    
    exec("/usr/local/share/Proc4/bin/EILoader $sapp $logdir/EIL_$sapp",
         $message, $status);

    if (!$status) {
        header("Refresh: 30; URL=\"EIBuilder.php?lockfile=$lockfile&logfile=$logfile\"");
    }
        
    ?>

    <pre><?php echo  $message?></pre>
<?php
} else if ($_SERVER['REQUEST_METHOD'] == 'GET' &&
           !is.null($_GET['lockfile'])) {
    $lockfile = $_GET['lockfile'];
    $logfile = $_GET['logfile'];
    if (file_exists($lockfile)) {
        header("Refresh: 15;
URL=\"EIBuilder.php?lockfile=$lockfile&logfile=$logfile\"");
        echo "<p>Loader is still running.</p>";
    } else {
        echo "<p>Loader has finished</p>";
    }
?>
<pre><?php `cat $logfile` ?></pre>
<?php
}
?>
<h2>Links to Other Pages</h2>
<ul>
    <li> <a href=Status.php">status</a> page.</li>
    <li> <a href=Shutdown.php">Shutdown</a> page.</li>
    <li> <a href=EIBuilder.php">Evidence Identification (EI)
        Builder (Loader)</a>.</li>
    <li> <a href=EABuilder.php">Evidence Accumulation (EA)
        Net Builder</a>.</li>
    <li> <a href=EIEvent.php">Evidence Identification (EI)
        Launcher</a>.</li>
    <li> <a href=EABN.php">Evidence Accumulation (EA)
        Launcher</a>.</li>
</ul>
</body>
</html>

<?php
} elseif($_SERVER['REQUEST_METHOD']=="OPTIONS") {
    header('Access-Control-Allow-Credentials: true');
    header('Access-Control-Allow-Headers: Content-Type, Accept, access-control-allow-credentials, access-control-allow-headers, access-control-allow-methods, access-control-allow-origin, access-control-max-age, X-Access-Token, X-Application-Name, X-Request-Time, X-Powered-by');
    header('Access-Control-Allow-Methods: GET, POST, OPTIONS');
    header('Access-Control-Allow-Origin: *');
    header('Access-Control-Max-Age: 1728000');
    header('Content-Length: 0');
    header('Content-Type: text/plain');
} else {
    die("This script only works with GET and POST requests.");
}
?>

