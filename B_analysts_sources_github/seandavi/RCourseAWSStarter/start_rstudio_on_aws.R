library(aws.ec2)
library(ssh.utils)
s <- describe_subnets()[[1]]
g <- create_sgroup("biocAMI_group", "new security group", vpc = s)
authorize_ingress(g, cidr = '0.0.0.0/0', port = 80, protocol = "tcp")
authorize_ingress(g, cidr = '0.0.0.0/0', port = 22, protocol = "tcp")
authorize_ingress(g, cidr = '0.0.0.0/0', port = 3838, protocol = "tcp")
keypair_name = 'bioc2017'
keypair_pem  = paste0(keypair_name,'.pem')
create_keypair(keypair_name,path = keypair_pem)
Sys.chmod(keypair_pem,mode = '600')


imageid = 'ami-a4697eb2'
# Launch the instance using appropriate settings
i <- run_instances(image = imageid, 
                   #type = "t2.micro", # <- you might want to change this
                   type = 'm4.10xlarge',
                   subnet = s, 
                   sgroup = g,
                   keypair = 'bioc2017')
str(describe_instances(i))
dnsName = describe_instances(i)[[1]]$instancesSet[[1]]$dnsName
Sys.sleep(120)
ssh_cmd = function(hostname,identity_file,cmd) {
    system(sprintf("ssh -oStrictHostKeyChecking=no -i %s ubuntu@%s '%s'",
                   identity_file, hostname, cmd))
}

# check connection
ssh_cmd(dnsName,'bioc2017.pem','uname')

copy_file_remote = function(identity_file,hostname,fname,remote_fname=basename(fname)) {
    system(sprintf("scp -oStrictHostKeyChecking=no -i %s %s ubuntu@%s:%s ",
                   identity_file, fname, hostname, remote_fname))
}
copy_file_remote('bioc2017.pem',dnsName,'install_shiny_server.sh')

ssh_cmd(dnsName,'bioc2017.pem','chmod +x install_shiny_server.sh')

useradd_ssh = function(hostname,user,identity_file,password=user) {
    system(sprintf("ssh -oStrictHostKeyChecking=no -i %s ubuntu@%s 'sudo useradd -m -p $(echo %s | openssl passwd -1 -stdin) %s'",
                   identity_file, hostname, user, password))
}

library(babynames)
library(dplyr)
unames = babynames %>% 
    filter(year>2009) %>% 
    group_by(name) %>% 
    summarize(x=sum(n)) %>% 
    arrange(desc(x)) %>% 
    transform(name=tolower(name)) %>%
    head(300) %>%
    pull(name)
sapply(unames,function(x) useradd_ssh(dnsName,x,'bioc2017.pem'))

ssh_cmd(dnsName,'bioc2017.pem','sudo ./install_shiny_server.sh')


# Stop and terminate the instances
stop_instances(i[[1]])
# terminating an instance will result in losing
# any and all data!
terminate_instances(i[[1]])
# and cleanup our security group.
Sys.sleep(120)
delete_sgroup(g)
delete_keypair(keypair_name)

