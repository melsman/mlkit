## Constructing a vagrant box

First move to the relevant directory and execute the following commands:

````
$ cd ~/gits/mlkit/ubuntu_i686_mlkit/
$ vagrant destroy
$ vagrant up
$ vagrant package
````

You have now created a file `package.box` in the directory
`~/gits/mlkit/ubuntu_i686_mlkit/`.

Now access the web address

````
https://app.vagrantup.com/melsman/boxes/ubuntu_1604_i686_mlkit/versions/
````

and create a new version X.Y.Z (provider virtualbox). You should now
be able to upload the `package.box` file.