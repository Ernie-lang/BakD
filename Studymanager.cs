using UnityEngine;
using UnityEngine.SceneManagement;
using System.Collections;
using System.Collections.Generic;

public class StudyManager : MonoBehaviour
{
    public static StudyManager Instance;

    // Latin square for 6 conditions (balanced order for up to 30 participants)
    private static int[,] latinSquare = new int[,]
    {
        { 0, 1, 2, 3, 4, 5 },
        { 1, 2, 3, 4, 5, 0 },
        { 2, 3, 4, 5, 0, 1 },
        { 3, 4, 5, 0, 1, 2 },
        { 4, 5, 0, 1, 2, 3 },
        { 5, 0, 1, 2, 3, 4 },
        { 0, 2, 4, 1, 3, 5 },
        { 1, 3, 5, 2, 4, 0 },
        { 2, 4, 0, 3, 5, 1 },
        { 3, 5, 1, 4, 0, 2 },
        { 4, 0, 2, 5, 1, 3 },
        { 5, 1, 3, 0, 2, 4 },
        { 0, 3, 5, 4, 2, 1 },
        { 1, 4, 0, 5, 3, 2 },
        { 2, 5, 1, 0, 4, 3 },
        { 3, 0, 2, 1, 5, 4 },
        { 4, 1, 3, 2, 0, 5 },
        { 5, 2, 4, 3, 1, 0 },
        { 0, 4, 3, 5, 1, 2 },
        { 1, 5, 4, 0, 2, 3 },
        { 2, 0, 5, 1, 3, 4 },
        { 3, 1, 0, 2, 4, 5 },
        { 4, 2, 1, 3, 5, 0 },
        { 5, 3, 2, 4, 0, 1 },
        { 0, 5, 1, 2, 3, 4 },
        { 1, 0, 2, 3, 4, 5 },
        { 2, 1, 3, 4, 5, 0 },
        { 3, 2, 4, 5, 0, 1 },
        { 4, 3, 5, 0, 1, 2 },
        { 5, 4, 0, 1, 2, 3 },
    };

    // Scene names matching your Scenes folder
    private string[] roomScenes = new string[]
    {
        "Room_Square_1x1",
        "Room_GoldenRatio_1x1618",
        "Room_Palladio_3x4",
        "Room_Palladio_2x3",
        "Room_Palladio_1x2",
        "Room_Extreme_1x3"
    };

    [HideInInspector] public int participantID = 0;
    [HideInInspector] public int currentRoomIndex = 0;
    [HideInInspector] public int[] roomOrder;

    void Awake()
    {
        // Singleton — persist across scenes
        if (Instance == null)
        {
            Instance = this;
            DontDestroyOnLoad(gameObject);
        }
        else
        {
            Destroy(gameObject);
        }
    }

    void Start()
    {
        StartStudy(11);
    }

    public void StartStudy(int participantID)
    {
        this.participantID = participantID;
        this.currentRoomIndex = 0;

        // Get this participant's room order from Latin square
        int row = participantID % 30;
        roomOrder = new int[6];
        for (int i = 0; i < 6; i++)
            roomOrder[i] = latinSquare[row, i];

        Debug.Log("Participant " + participantID + " room order: " +
            roomOrder[0] + ", " + roomOrder[1] + ", " + roomOrder[2] + ", " +
            roomOrder[3] + ", " + roomOrder[4] + ", " + roomOrder[5]);

        LoadNextRoom();
    }

    public void LoadNextRoom()
    {
        if (currentRoomIndex >= 6)
        {
            // All rooms done
            SceneManager.LoadScene("EndScene");
            return;
        }

        int sceneIndex = roomOrder[currentRoomIndex];
        string sceneName = roomScenes[sceneIndex];
        currentRoomIndex++;

        StartCoroutine(FadeAndLoad(sceneName));
    }

    private IEnumerator FadeAndLoad(string sceneName)
    {
        // Fade out
        if (FadeController.Instance != null)
            yield return StartCoroutine(FadeController.Instance.FadeOut());

        SceneManager.LoadScene(sceneName);
    }
}