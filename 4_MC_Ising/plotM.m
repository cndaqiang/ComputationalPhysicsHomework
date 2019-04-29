%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   ___                               _            _     _                       _  %%%
%%%  / __|  ___   _ __    _ __   _  _  | |_   __ _  | |_  (_)  ___   _ _    __ _  | | %%%
%%% | (__  / _ \ | '  \  | '_ \ | || | |  _| / _` | |  _| | | / _ \ | ' \  / _` | | | %%%
%%%  \___| \___/ |_|_|_| | .__/  \_,_|  \__| \__,_|  \__| |_| \___/ |_||_| \__,_| |_| %%%
%%%  ___   _             |_|  _                                                       %%%
%%% | _ \ | |_    _  _   ___ (_)  __   ___                                            %%%
%%% |  _/ | ' \  | || | (_-< | | / _| (_-<                                            %%%
%%% |_|   |_||_|  \_, | /__/ |_| \__| /__/                                            %%%
%%%  _  _         |__/                               _                                %%%
%%% | || |  ___   _ __    ___  __ __ __  ___   _ _  | |__                             %%%
%%% | __ | / _ \ | '  \  / -_) \ V  V / / _ \ | '_| | / /                             %%%
%%% |_||_| \___/ |_|_|_| \___|  \_/\_/  \___/ |_|   |_\_\                             %%%
%%%                                                                                   %%%
%%% Author:       cndaqiang                                                           %%%
%%% ContactMe:    https://cndaqiang.github.io                                         %%% 
%%% Name:         plotM                                                               %%%
%%% Last-update:  2019-04-26                                                          %%%
%%% Build-time:   2019-03-29                                                          %%%
%%% What it is:   MATLAB read data and Plot S(i,j)                                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for T = [3.5:-0.1:1.5]
    filename=['down.',num2str(T,'%2.1f\n'),'.Mesh.1.dat'];
    M=importdata(filename);
    M=-0.5*(M-1);
    imshow(M,'InitialMagnification','fit') %0 white 1 black
    titlename=['T = ',num2str(T,'%2.1f\n')]
    title(titlename);
    pngname=['down.',num2str(T,'%2.1f\n'),'.png'];
    saveas(gcf,pngname)
    pause(0.5)
end

for T = [1.6:0.1:3.5]
    filename=['up.',num2str(T,'%2.1f\n'),'.Mesh.1.dat'];
    M=importdata(filename);
    M=-0.5*(M-1);
    imshow(M,'InitialMagnification','fit')
    titlename=['T = ',num2str(T,'%2.1f\n')]
    title(titlename);
    pngname=['up.',num2str(T,'%2.1f\n'),'.png'];
    saveas(gcf,pngname)
end
