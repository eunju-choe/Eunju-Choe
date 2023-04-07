import pandas as pd
import random as rd


def ghost__OX(inputs, n_outputs, return_only_O=False):
    """
    사다리타기 1 : 당첨자 정하기
    입력 목록에서 입력한 수만큼 당첨자를 선택하여 데이터프레임으로 반환합니다.

    Args:
        inputs (list): 사다리타기 입력값
        n_outputs (int): 당첨자의 수
        return_only_O (bool, default=False): 당첨자만 출력할지에 대한 여부

    Returns:
        dataframe: 사다리타기 결과값
    """
    result_df = pd.DataFrame()

    while True:
        # input이 list 형태가 아닌 경우
        if type(inputs) != list:
            print('!! inputs는 list의 형태로 입력해주세요 !!')
            break

        # 입력 목록의 개수 세기
        n_inputs = len(inputs)

        # 빈 list를 입력한 경우
        if n_inputs < 1 :
            print('!! 입력 목록이 비어있습니다 !!')
            break

        # 입력 목록의 수 < 당첨자의 수
        if n_inputs < n_outputs:
            print('### 입력 목록의 수보다 당첨자의 수가 많습니다 ###')

        # 당첨자의 수가 0인 경우
        if n_outputs == 0:
            print('### 당첨자의 수가 0명입니다 ###')

        # 출력 리스트 생성
        try:
            outputs = ['당첨' for x in range(n_outputs)] + ['꽝' for x in range(n_inputs - n_outputs)]
        except TypeError:
            print('!! 당첨자의 수는 int형태로 입력되어야 합니다 !!')
            break

        # 사다리 타기 결과
        res_dict = {}
        for x in inputs:
            res = rd.choice(outputs)
            outputs.remove(res)
            res_dict[x] = res

        # 데이터프레임으로 변환
        result_df = pd.DataFrame([res_dict], index=['result']).T

        # 당첨자만 출력        
        if return_only_O:
            result_df = result_df[result_df['result']!='꽝']

        break

    return result_df


def ghost_leg_role(inputs, outputs, return_only_O=False):
    """
    사다리타기 2 : 역할 배정하기
    입력 목록과 역할 목록을 직접 입력하여 사다리타기한 결과를 반환합니다.

    Args:
        inputs (list): 사다리타기 입력값
        outputs (list): 사다리타기 출력값
        return_only_O (bool, default=False): 당첨자만 출력할지에 대한 여부

    Returns:
        dataframe: 사다리타기 결과값
    """
    result_df = pd.DataFrame()

    while True:
        # inputs 또는 outputs가 list 형태가 아닌 경우
        if (type(inputs) != list) or (type(outputs) != list):
            print('!! inputs와 outputs는 list의 형태로 입력해주세요 !!')
            break

        # inputs에 빈 list를 입력한 경우
        if len(inputs) < 1 :
            print('!! 입력 목록이 비어있습니다 !!')
            break

        # 입력목록의 개수보다 출력목록의 개수가 많은 경우
        if len(inputs) < len(outputs):
            print('!! 입력 목록의 수보다 출력 목록의 수가 많습니다 !!')
            break

        # outputs에 빈 list를 입력한 경우
        if (len(outputs)) < 1:
            print('### 출력 목록이 비어있습니다 ###')

        # 출력 리스트 생성
        outputs = outputs + ['꽝' for x in range(len(inputs) - len(outputs))]

        # 사다리 타기 결과
        res_dict = {}
        for x in inputs:
            res = rd.choice(outputs)
            outputs.remove(res)
            res_dict[x] = res

        # 데이터프레임으로 변환
        result_df = pd.DataFrame([res_dict], index=['result']).T

        # 당첨자만 출력        
        if return_only_O:
            result_df = result_df[result_df['result']!='꽝']

        break

    return result_df


def ghost_leg_order(inputs, n_outputs, return_only_O=False, order=True):
    """
    사다리타기 3 : 순서 정하기
    입력 목록에서 입력한 수만큼 순서를 정하고 데이터프레임으로 반환합니다.

    Args:
        inputs (list): 사다리타기 입력값
        n_outputs (int): 당첨자의 수
        return_only_O (bool, default=False): 당첨자만 출력할지에 대한 여부
        order (bool, default=True): 순위에 따라 정렬할지에 대한 여부

    Returns:
        dataframe: 사다리타기 결과값
    """
    result_df = pd.DataFrame()

    while True:
        # input이 list 형태가 아닌 경우
        if type(inputs) != list:
            print('!! inputs는 list의 형태로 입력해주세요 !!')
            break

        # 입력 목록의 개수 세기
        n_inputs = len(inputs)

        # 빈 list를 입력한 경우
        if n_inputs < 1 :
            print('!! 입력 목록이 비어있습니다 !!')
            break

        # 입력 목록의 수 < 당첨자의 수
        if n_inputs < n_outputs:
            print('!! 입력 목록의 수보다 당첨자의 수가 많습니다 !!')
            break

        # 당첨자의 수가 0인 경우
        if n_outputs == 0:
            print('### 당첨자의 수가 0명입니다 ###')

        # 출력 리스트 생성
        try:
            outputs = [str(x+1) for x in range(n_outputs)] + ['꽝' for x in range(n_inputs - n_outputs)]
        except TypeError:
            print('!! 당첨자의 수는 int형태로 입력되어야 합니다 !!')
            break

        # 사다리 타기 결과
        res_dict = {}
        for x in inputs:
            res = rd.choice(outputs)
            outputs.remove(res)
            res_dict[x] = res

        # 데이터프레임으로 변환
        result_df = pd.DataFrame([res_dict], index=['result']).T

        # 당첨자만 출력        
        if return_only_O:
            result_df = result_df[result_df['result']!='꽝']

        # 순서에 따라 정렬
        if order:
            result_df = result_df.sort_values(by='result', ascending=True)

        break

    return result_df


def make_team(name_list, n_list, team_names=None):
    """
    팀 만들기 : 이름 목록(name_list)과 팀별 인원 목록(n_list)를 입력하면 인원에 맞게 팀을 자동으로 구성합니다.

    Args:
        name_list (list): 이름 목록
        n_list (list): 팀별 인원 목록
        team_names (list, optional): 팀명 목록. Defaults to None.

    Returns:
        pd.DataFrame: 팀 구성 결과
    """
    result_df = pd.DataFrame()

    while True:
        # input이 list의 형태가 아닌 경우
        if (type(name_list) != list) or (type(n_list) != list):
            print('!! name_list와 n_list는 list의 형태로 입력해주세요 !!')
            break

        # team_names가 list의 형태가 아닌 경우
        if (team_names != None) & (type(team_names) != list):
            print('### team_names는 list의 형태로 입력해주세요 ###')
            team_names = range(len(n_list))

        # name_list의 길이와 n_list의 합이 일치하지 않는 경우
        if len(name_list) != sum(n_list):
            print('!! 입력한 이름 목록의 개수와 인원 목록의 합이 일치하지 않습니다 !!')
            break

        # team_names의 길이와 n_list의 길이가 일치하지 않는 경우
        if team_names != None:
            if len(n_list) != len(team_names):
                print('### 구성할 팀의 개수와 team_names의 개수가 일치하지 않습니다 ###')
                team_names = range(len(n_list))

        # 팀 구성
        result = []
        for i in range(len(n_list)):
            members = []
            for _ in range(n_list[i]):
                member = rd.choice(name_list)
                members.append(member)
                name_list.remove(member)

            result.append([members])

        # 데이터 프레임 형태로 변환
        result_df = pd.DataFrame(result, index=team_names, columns=['name'])
        break

    return result_df