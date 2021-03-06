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
%%% Name:         find_root_module_function                                           %%%
%%% Last-update:  2019-06-21                                                          %%%
%%% Build-time:   2019-06-21                                                          %%%
%%% What it is:    样条插值                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
figure(1)
clf
x=-1:0.5:1;
y=1./(1+x.^2);
%plot(x,y,'rdiamond','linestyle', 'none')
hold on

%% 三次样条插值
xx=-1:0.01:1;
yy = spline(x,y,xx);
result=spline(x,y);
plot(xx,yy,'g')

%% 精确值
ry=1./(1+xx.^2);
plot(xx,ry,'r',x,y,'r^')
legend('分段三次样条插值','实际值')
%% 结果
disp(['断点:  ',num2str(result.breaks)])
disp(['分段三次插值函数系数'])
disp([num2str(result.coefs)])


%% 2. 
x=[0.3 0.5 0.6 0.7 0.9];
y=[1.37731 1.48766 1.53879 1.58653 1.67];
