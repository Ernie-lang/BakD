using UnityEngine;
using UnityEngine.XR;
using System.Collections;

public class RoomController : MonoBehaviour
{
    private bool wasPressed = false;

    void Start()
    {
        StartCoroutine(FadeInOnStart());
    }

    private IEnumerator FadeInOnStart()
    {
        yield return new WaitForSeconds(0.1f);
        if (FadeController.Instance != null)
            yield return StartCoroutine(FadeController.Instance.FadeIn());
    }

    void Update()
    {
        InputDevice rightHand = InputDevices.GetDeviceAtXRNode(XRNode.RightHand);
        rightHand.TryGetFeatureValue(CommonUsages.primaryButton, out bool isPressed);

        if (isPressed && !wasPressed)
        {
            ExitRoom();
        }

        wasPressed = isPressed;

        // Keyboard fallback
        if (Input.GetKeyDown(KeyCode.Space))
        {
            ExitRoom();
        }
    }

    void ExitRoom()
    {
        if (StudyManager.Instance != null)
            StudyManager.Instance.LoadNextRoom();
    }
}