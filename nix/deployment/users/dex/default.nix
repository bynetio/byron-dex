{ pkgs, ... }:
{
  users.users.dex = {
    description  = "Dex Application";
    extraGroups  = [ "docker" "systemd-journal" "wheel" ];
    isNormalUser = true;
    uid          = 1001;

    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDrSau4Jlq3xQNiiEMkgETh6bU0/gSlG7ecOFOhzNrcYtcLBQzKNfJrk/59JmXNxXws3u3RBYk1oCe3xnCdeqSTpj4sLJEfXHBuGR4hk2kdk1ve+A0SxL2RKMEGUuA8v0O/0oRykv1EV3oh8HwfYVj0AQzHNxSk1H815gPGNRaq9OTJJgQvUtjNx09dtdY071rNV3D5/ozUqGczdeRbvSlSCHkLZ9mHFGJxd9lbfMV6Bs/XxHrHg+Tc3HDOSmJq7UZeX9i0kvKdyGz9qFdhuIZL4nJWrRjbAMgvMGJJxohtdqgrMv9xuz5UveNVotWBojrMU6n4UcgB1ugUkrDmDL1aBJP6zeRcgk5CtisSMt2eq69LmBEwZDWNHqVQg2Kft32urOH82VfEeZLT+sXD1kWvCFVRcmZtZlENmmkqr0axp9gf4mg1IBkyM7eXjxTg1lDeDw5yFVG/cfbtOUc+twWFJ7nFlC6wVE5prnRW+qI6gpGB4gGZVtzODmIT4OeTXKI2MZPTMn2pwjmx3NM3p8ofZawr3c8TZwCStuWiIvoes3Ps4kt2Z75hoZ+4+LEucUwop0jees0YxrNoFTbwdbfXH0mBCspeSS65CZ96Og2qdE7s1+t3tdZrBWPmgziZIPtvBAmYmzH9JKAX1JgmRirf4tG5sZ2JbA8WDUqSADmadw== cardno:000611879902" # placek
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC1LA8IMe3QoMZTb3K5uiaS6b2ofUrcNeK+HITmJ7iMWSAPQn9we7BjdWWktEKQ/B+3zEL+yE55IiW3u1X5zzmwWIzWCrLmVxh9UENUMLmHI9dAnXxuWLRsGkF94mLCmybNN6Ebrp1aQbOMGsnXjSANoTt1YDNHHKcF58yiXZgi279r8kJMhwwqu+GsFYGBGA3tUsz7UBlKzgHzNYWcVbQHbmBrRbtQo5Qx4+Wxw24Ms9OFmqNTn0uQ0dqJRCTAT/I6QJrVV9WVBD++ShV7/Sw2X5hAknWj0l+pk7d3kGn3UZvLZ1prlL3MJciQnIK5jHcvvEG6XcdXUZui7UHijcNl slawomir.sledz@gmail.com" # Slavik
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHm5FAv9Tr1ycXOkeaM/qfyZpOlAFZpkKQhoUwFIUwB8 jonatan.borkowski@pm.me" # jonatan
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC8nmMp9ZIuMx0lhEXWRJq6deSRtNTILOebrKoDYF4ZOI0tIpF3Cx8rCdqts80/lq9tzK5Of0rYru2VeqI1TrH/1xl/NswSVPu3m76HsWg5nfdnARBaU/DAydFeqTPI+r1pUTg7yC/duQotncP+QJ3nzlmHbtDeppKr8drZPnXdFdFIsSSmrvfmAOFlfgBLhxAt3/c0tIvq+w/q1P6A7abtg8NOad8fwCG1fsNEy/vTQwm8NblmPy86TVhn1XoJpFWICQ9zmspMeWqiS5l62O5gpcav5TcT1MowGDwABoUSsMJSRP4g+MPT2yL/xZXOGzRGymTN/1OHYzXgtJ6d2r6HHUj7ZX2T+jEd12fI7jaE+mlwn1CLJnI4+1KXApVLGtVbju2ApGebxok5V1PhDbVMzST/JupyFoogWocap3BXfB0YL3hkxNSS4BpLqQKumVP9Lu92zhrRUHXyjjkgH1tm0rmGL6qyVyIWDh4RoONsyhg2jZdk3q+cGIOKUTyWoLk= michaljankun@MacBook-Air-Micha.local" # jankun
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDGLMol8ykNIN890e9C+xWEU3/2SLJBVtcM9TAgKiieBirpEgbMH+D+qbgJLg/0syqKolIEwetR9L1edDSLgREYszwpoMCZqabdvJAsDaghUb9+3AwuY++1UcDRLuyOE+/ODdbtFjL3ZEeWPeCJHVwQoXgHpFOR1+44sI5LNZdcSRWiABtrcNGQ1+xHPsTTlh0ALAQYXUbmi9Q46HvQ4qH+oldT/hitFjoO2ixw8a8JByF/QZm5pamhTkicPow0iYIN91AyfuMfjYGnOmGpmlrSC1CnYtsY4f7MtOdYDvFyVb5vgMICp6YG+Y1X/U1aUWJhlvfl4KaS6f/cd0u9rw/x/jVyHWP8RyctF56CYSPF5bU2cS/Kn7sKqzK1ePlv0MbtTg2Lx5L94s3+/urrD+my8oSlhIs2nClUg7ktZbEWLM8bTBihvT5zo1AazD2F5YuTsK1AY0hVN0mHH+/lPO3YaY9LAjyhj5cbLRbcrgNVmPheb0/g2EpnlEILISnoS9k= rafal@fedora" # rafał
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDgheTU9pEPQ9XylE0ZpY+t3Cc42P5VLEQO5HKxpGniWHYt8wF0axONxVJoT7922CIn+NXTumcaPMufh8WS6Q/o1Cm5MetBprB9ky5rttGN/jALYHLuhUHIjIZXXtymyzKo8CHNdY1PUCNAdcW2mnVVLZPJITf2LvNxxAPd6hb4ZytkUt5nRjrERcYcTzrrEpPT7lNa7Rvw+Hges/RDgoZr5+lFUjFhNFiQYD0hnkek4kyLiL0HlwwA3EQkK+F1hkeOug+DQdUdJQKJ1v3aFNyPLeiiESOGoF6OmK61DiC+ChUS+AfjPub2wFI8JJ2LyA0V3ofoyWIaDuxJK3Xpel8lQuwRKArNl9fmQ5r0LyDLyvqvDS/oRpksWcI5eN9MyVD2L4/oEp05iBne6h6r/fKx2eMD5ERlw6794ooP0vZs5bRMKClJP7vEq624qrpDEU5rvQC7j3d8P8XJ1vCRpYx712915Lb8P9oCCwoo4RBLS9kcgFijHb3pVqNx5yJojhc= tomasz.holubowicz@binarapps.com" # tomek
    ];

    # packages available only for dex
    packages = with pkgs; [
      jq
      wget
    ];
  };
}
